
import ShowWords

-- Usage: ./show_words mode file [column_names]
--
--   Show words (phrases) from file one by one in specified column order and
-- check your answers against next word.
--
--      'mode' - operation mode:
--          - "print" for waiting for a key after each phrase in specified
--            columns,
--          - "check" for checking user input against each next phrase in
--            specified columns (literal check).
--      'file' - file with words.
--      [column_names] - any number of any column names (may be prefixes), one
--                       in one command line argument.
--
--   File must contain lines of words. Line may contain several columns
-- separated by columnSep. Column may contain several phrases separated by
-- phraseSep.  First line of file treated as heading (reference), and should
-- contain column names in current order separated by referenceSep.
--
--   File will be displayed line by line in the specified columns order with
-- leading and trailing spaces of each column and each phrase deleted (inner
-- spaces preserved). You may specify desired column order at command line -
-- any column names (or just prefixes of column names) in any order.
--
--   If there is at least two valid column names specified, first is treated
-- as "question". Before every phrase in other (not first) specified columns
-- requested action (wait or check) will be executed. All other
-- (non-specified) columns will be outputted at once right after last
-- specified column. No action will be executed before them or phrases in
-- them, and execution immediately proceeds at the next line.
--   Hence, if there is less, than two columns specified, entire file will be
-- outputted at once, so this has a little sense :-)
--
--   Incorrect column names silently skipped without any notification.  This
-- may lead to nasty bugs, when all seems ok, but not works however. So,
-- double check column names in words file and on cmd!
main :: IO ()
main                = showWords WordsSeps   { columnSep = " : "
                                            , phraseSep = ", "
                                            , referenceSep = " - "
                                            }

