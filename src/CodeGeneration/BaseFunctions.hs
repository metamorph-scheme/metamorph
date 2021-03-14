module CodeGeneration.BaseFunctions where

import qualified Data.Map as M
import Data.Maybe (fromJust)

baseFunction :: String -> String
baseFunction = fromJust . (flip M.lookup) (M.fromList [
    ("current-input-port", "current_input_port"),
    ("current-output-port", "current_output_port"),
    ("current-error-port", "current_error_port"),
    ("read-char", "read_char"),
    ("peek-char", "peek_char"),
    ("read-line", "read_string"),
    ("newline", "newline"),
    ("write-char", "write_char"),
    ("write-string", "write_string"),
    ("flush-output-port", "flush_output_port")
  ])