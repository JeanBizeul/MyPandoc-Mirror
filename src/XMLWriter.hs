{-
-- EPITECH PROJECT, 2025
-- MyPandoc-Mirror
-- File description:
-- XMLWriter
-}

module XMLWriter (
    documentToXML
) where

import GeneralParse
import Document
import Data.Char
import Data.List
import Data.Maybe

writeXMLToFile :: String -> FilePath -> IO ()
writeXMLToFile content filePath = writeFile filePath content

documentToXML :: Document -> String
documentToXML (Document h b) =
  "<document>\n" ++ headerToXML h ++
    concatMap contentToXML b ++ "\t</body>\n</document>"

headerToXML :: Header -> String
headerToXML (Header title mAuthor mDate) =
  "\t<header title=\"" ++ title ++ "\">"
  ++ maybe "" (\a -> "<author>" ++ a ++ "</author>") mAuthor
  ++ maybe "" (\(Date d) -> "<date>" ++ d ++ "</date>") mDate
  ++ "</header>\n\t<body>\n"

contentToXML :: Content -> String
contentToXML (Text s)              = s
contentToXML (TitleContent t)      = "\t\t" ++ titleToXML t
contentToXML (Formatted ft _)      = "\t\t" ++ formatToXML ft
contentToXML (LinkContent l)       = "\t\t" ++ linkToXML l
contentToXML (ImageContent i)      = "\t\t" ++ imageToXML i
contentToXML (ParagraphContent p)  = "\t\t" ++ paragraphToXML p
contentToXML (SectionContent s)    = "\t\t" ++ sectionToXML s
contentToXML (CodeBlockContent c)  = "\t\t" ++ codeBlockToXML c
contentToXML (ListContent l)       = "\t\t" ++ listToXML l

titleToXML :: Title -> String
titleToXML (Title t level) =
  "<title level=\"" ++ show level ++ "\">" ++ t ++ "</title>\n"

formatToXML :: FormatType -> String
formatToXML (Italic c) = "<italic>" ++ concatMap contentToXML c ++ "</italic>"
formatToXML (Bold c)   = "<bold>"   ++ concatMap contentToXML c ++ "</bold>"
formatToXML (Code s)   = "<code>"   ++ s ++ "</code>"

linkToXML :: Link -> String
linkToXML (Link label url) =
  "<link url=\"" ++ url ++ "\">"
  ++ concatMap contentToXML label ++ "</link>\n"

imageToXML :: Image -> String
imageToXML (Image alt url) =
  "<image alt=\"" ++ alt ++ "\" url=\"" ++ url ++ "\"/>\n"

paragraphToXML :: Paragraph -> String
paragraphToXML (Paragraph content) =
  "<paragraph>" ++ concatMap contentToXML content ++ "</paragraph>\n"

sectionToXML :: Section -> String
sectionToXML (Section mTitle body) =
  "<section>\n\t\t\t"
  ++ maybe "" titleToXML mTitle
  ++ concatMap contentToXML body
  ++ "\t\t</section>\n"

codeBlockToXML :: CodeBlock -> String
codeBlockToXML (CodeBlock code mLang) =
  "<codeblock" ++ maybe "" (\l -> " language=\"" ++ l ++ "\"") mLang ++ ">"
  ++ concatMap contentToXML code ++ "</codeblock>\n"

listToXML :: List -> String
listToXML (List items) =
  "<list>\n" ++ concatMap itemToXML items ++ "\t\t</list>\n"

itemToXML :: Item -> String
itemToXML (Item content) =
  "\t\t\t<item>" ++ concatMap contentToXML content ++ "</item>\n"
