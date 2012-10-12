-- | This library provides functions to SMSDirect API.
--
-- Simple usage:
--
-- @
-- -- Get DB list
-- r <- smsdirect \"test\" \"test\" getDB
--
-- -- Make url for tracing purposes
-- let cmd = url \"test\" \"test\" getDB
-- @
--
module SMSDirect (
    module SMSDirect.Command
    ) where

import SMSDirect.Command
