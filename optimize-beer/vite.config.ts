import { defineConfig } from 'vite'

export default defineConfig({
  build: {
    manifest: true,
    outDir: 'static',
    rollupOptions: {
      input: 'frontend/main.ts',
    },
  },
  server: {
    origin: 'http://localhost:5173',
    strictPort: true,
  },
})
