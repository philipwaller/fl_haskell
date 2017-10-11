-- make people noble
mknoble :: Bool -> String -> String
-- mknoble (True ,name) = "Dame " ++ name
-- mknoble (_,name) = "Sir " ++ name


mknoble female name = if female then "Dame" else "Sir " ++ name
