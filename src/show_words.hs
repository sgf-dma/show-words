
import ShowWords

-- Usage: ./show_words mode file [column_names]
--
--      mode - operation mode:
--          - "print" for waiting for a key after each specified column,
--          - "check" for checking user input against each phrase in next
--          specified columns (only literal check supported yet).
--      file - file with words.
--      [column_names] - any number of any column names, one in one cmd
--      argument.
--
--   Show words from file one by one in specific order and check your answers
-- against next word.
--
--   File must contain lines of words. Line may contain several columns
-- separated by dash. First line of file treated as heading (reference), and
-- should contain column names in current order. Leading and trailing spaces
-- in each column will be deleted (inner column spaces preserved).
--
--   File will be displayed line by line in the specified columns order. You
-- may specify desired column order at cmd - any column names in any order.
--   If there is at least two valid column names specified, first is treated
-- as "question". Before every other specified column requested action (wait
-- or check) will be executed. All other (non-specified) columns will be
-- outputted at once right after last specified column. No action will be
-- executed before them, and execution immediately proceeds at the next line.
--   Hence, if there is less, than two columns specified, entire file be
-- outputted at once, so this has a little sense :-)
--   Incorrect column names silently skipped without any notification.  This
-- may lead to nasty bugs, when all seems ok, but not works however. So,
-- double check column names in words file and on cmd!
main :: IO ()
main                = showWords WordsSeps   { columnSep = " : "
                                            , phraseSep = ", "
                                            , referenceSep = " - "
                                            }

