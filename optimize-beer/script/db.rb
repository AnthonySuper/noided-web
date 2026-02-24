#!/usr/bin/env ruby

require "bundler"
require "pathname"
require "open3"
require "logger"
require "yaml"

Bundler.setup
Bundler.require

$logger = Logger.new($stdout)

class DBConfigs
  def initialize(path)
    @configs = YAML.load_file(path).transform_values do |val|
      DBConfig.new(val)
    end
  end

  def databases_for(env, &)
    return enum_for(__method__, env) unless block_given?

    case env
    when nil, /development/i
      devdb = development_db
      $logger.info { "Yielding development database (#{devdb.describe})..." }
      yield devdb
      testdb = test_db
      $logger.info { "Yielding test database (#{testdb.describe})..." }
      yield testdb
    when /production/
      $logger.info { "Yielding production database..." }
      yield @configs.fetch("production")
    end
  end

  def development_db
    @configs.fetch("development")
  end

  def test_db
    @configs.fetch("test")
  end
end

class DBConfig
  def initialize(cfg)
    @cfg = cfg
  end

  def describe
    return 'from DATABASE_URL' if @cfg.nil?

    @cfg.inspect
  end

  ARGUMENT_MAPPING = {
    "-h" => "host",
    "-p" => "port",
    "-U" => "user",
    "--dbname" => "dbname",
    "-password" => "password"
  }

  SEQUEL_MAPPING = {
    "dbname" => :database
  }

  def pg_dump_args
    return [ENV.fetch("DATABASE_URL")] if @cfg.nil?

    ARGUMENT_MAPPING.flat_map do |k, v|
      val = @cfg[v]

      next [] if val.nil?

      [k, val]
    end
  end

  def with_connection
    prepare_sequel

    connected do |db|
      db.extension :pg_array
      db.extension :pg_enum
      yield db
    end
  end

  private

  def prepare_sequel
    Sequel.extension :migration

    mod = Sequel::Schema::CreateTableGenerator
    mod.include(MigrationExtensionCommands) unless mod <= MigrationExtensionCommands
  end

  def connected(&)
    case @cfg
    when nil
      Sequel.connect(ENV.fetch("DATABASE_URL"), logger: $logger, &)
    else
      Sequel.connect(**cfg_for_sequel, logger: $logger, adapter: :postgres, &)
    end
  end

  def cfg_for_sequel
    @cfg.map do |k, v|
      new_key = SEQUEL_MAPPING.fetch(k) { k.to_sym }
      [new_key, v]
    end.to_h
  end

  def logger = Logger.new($stdout)
end

module MigrationExtensionCommands
  def timestamps
    %i[created_at updated_at].each do |col|
      timestamptz(
        col,
        null: false,
        default: Sequel.function(:now)
      )
    end
  end
end

class Commands < Thor
  desc "gen_migration", "generates a new DB migration"
  def gen_migration(name)
    FileUtils.mkdir_p(migrations_folder)
    time = Time.now.utc.strftime("%Y%m%d%H%M")
    filename = "#{time}_#{name}.rb"
    filepath = migrations_folder / Pathname(filename)
    puts filepath.to_s
    filepath.open('w') do |f|
      f.puts "Sequel.migration do"
      f.puts "  change do"
      f.puts "  end"
      f.puts "end"
    end
  end

  desc "migrate", "run migrations"
  def migrate
    with_database do |db|
      Sequel::Migrator.run(db, migrations_folder, use_transactions: true)
    end
    dump_schema unless environment&.match?(/production/i)
  end

  desc "rollback", "rollback the last migration"
  def rollback
    with_database do |db|
      filename = db[:schema_migrations]
        .order(Sequel.desc(:filename))
        .offset(1)
        .first
        .fetch(:filename)
      version = filename.match(/^\d+/).to_s.to_i
      Sequel::Migrator.run(db, migrations_folder, target: version, use_transactions: true)
    end
    dump_schema
  end

  desc "dump_schema", "dump the DB schema"
  def dump_schema
    $logger.info { "Dumping schema with args: #{pg_dump_args.inspect} "}
    schema, _ = Open3.capture2("pg_dump", "--schema-only", "--no-owner", *pg_dump_args)
    migrations, _ = Open3.capture2("pg_dump", "-t", "schema_migrations", "--section=data", "--column-inserts", "--rows-per-insert=1000", "--no-owner", *pg_dump_args)
    schema_file.open("w") do |stream|
      stream.puts(clean_pg_dump(schema + migrations))
    end
  end

  no_commands do
    def pg_dump_args = development_db.pg_dump_args
    def development_db = db_config.development_db
    def db_config = DBConfigs.new db_config_path
    def db_config_path = (Pathname(__FILE__) + Pathname("../../config/db.yml")).cleanpath
    def db_url = ENV.fetch("DATABASE_URL")
    def db_folder = (Pathname(__FILE__) + Pathname("../../db")).cleanpath
    def migrations_folder = db_folder + Pathname("migrations")
    def environment = ENV["OPTIMIZE_BEER_ENV"]
    def schema_file = db_folder + Pathname("schema.sql")
    def with_database(&)
      db_config.databases_for(environment).each { |db| db.with_connection(&) }
    end

    def clean_pg_dump(output)
      output
        .gsub(/^--\n-- PostgreSQL database dump( complete)?\n--\n/, "")
        .gsub(/^\\(un)?restrict \S+\n/, "")
        .gsub(/^-- Dumped (from|by) .+\n/, "")
        .gsub(/^SET \S.*\n/, "")
        .gsub(/^SELECT pg_catalog\.set_config\(.*\n/, "")
        .gsub(/\n{3,}/, "\n\n")
        .strip
    end
  end

  def self.exit_on_failure? = true
end

Commands.start
