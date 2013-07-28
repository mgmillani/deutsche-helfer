module Laden(loadData) where

separate line =
	let aux str =
		case str of
			(h:r) ->
				let (word,rest) = aux r in if h==' ' then ("",r)	else (h:word,rest)
			[] -> ("","")

	in
		let (genus,rest) = aux line
		    (word,_) = aux rest
		in
			(genus,word)


loadData filename = do
	file <- readFile filename

	let lns = lines file
	return $ filter (\(g,w) -> g == "der" || g == "die" || g=="das") (map separate lns)