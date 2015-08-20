module Propellor.Property.ConfFile (containsIniPair) where

import Propellor
import Propellor.Property.File

import Data.List (isPrefixOf, foldl')

-- | find the line that is the start of the wanted section (eg, == "<Foo>")
type SectionStart  = Line -> Bool
-- | find a line that indicates we are past the section
-- (eg, a new section header)
type SectionPast   = Line -> Bool
-- | run on all lines in the section, including the SectionStart line;
-- can add/delete/modify lines, or even delete entire section
type AdjustSection = [Line] -> [Line] 
-- | if SectionStart does not find the section in the file, this is used to
-- insert the section somewhere within it
type InsertSection = [Line] -> [Line]

adjustSection
	:: String
	-> SectionStart
	-> SectionPast
	-> AdjustSection
	-> InsertSection
	-> FilePath
	-> Property NoInfo
adjustSection desc start past adjust insert f =
	fileProperty desc go f
  where
	go ls = let (pre, wanted, post) = foldl' find ([], [], []) ls
		in if null wanted
			then insert ls
			else pre ++ (adjust wanted) ++ post
	find (pre, wanted, post) l
		| null wanted && null post && (not . start) l =
			(pre ++ [l], wanted, post)
		| (start l && null wanted && null post)
		  || ((not . null) wanted && null post && (not . past) l) =
			  (pre, wanted ++ [l], post)
		| otherwise = (pre, wanted, post ++ [l])

iniHeader :: String -> String
iniHeader header = '[' : header ++ "]"

adjustIniSection
	:: String
	-> String
	-> AdjustSection
	-> InsertSection
	-> FilePath
	-> Property NoInfo
adjustIniSection desc header =
	adjustSection
	desc
	(== iniHeader header)
	("[" `isPrefixOf`)

containsIniPair :: FilePath -> (String, String, String) -> Property NoInfo
containsIniPair f (header, key, value) =
	adjustIniSection
	(f ++ " section [" ++ header ++ "] contains " ++ key ++ "=" ++ value)
	header
	go
	(++ [confheader, confline])
	f
  where
	confheader = iniHeader header
	confline   = key ++ "=" ++ value
	go []      = [confline]
	go (l:ls)  = if isKeyVal l then confline : ls else l : (go ls)
	isKeyVal x = (filter (/= ' ') . takeWhile (/= '=')) x `elem` [key, '#':key]
