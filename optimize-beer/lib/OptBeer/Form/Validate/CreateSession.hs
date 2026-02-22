{-# LANGUAGE OverloadedStrings #-}

module OptBeer.Form.Validate.CreateSession where

import Control.Monad (when)
import Data.Text qualified as T
import Noided.Form.HKD
import Noided.Validation
import OptBeer.Form.Type.CreateSession
import OptBeer.Type.Hashword

-- | Simple validator for session creation.
-- Only checks that the fields are non-blank.
createSessionValidator :: (Monad m) => FormValidator m (SubformField CreateSessionF)
createSessionValidator = validateSubform $
  CreateSession
    { email = validateInput $ \t -> do
        when (T.null t) $ failNonfatal Blank
        return t,
      password = validateInput $ \pw -> do
        when (T.null pw.getOpaquePassword) $ failNonfatal Blank
        return pw
    }

