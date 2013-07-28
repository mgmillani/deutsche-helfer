{-
	This file is part of Deutsche Nummers.

    Deutsche Nummers is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Deutsche Nummers is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Deutsche Nummers.  If not, see <http://www.gnu.org/licenses/>.
 -}

module Nummer(schreibenNummer) where

algs = ["null","ein","zwei","drei","vier","fünf","sechs","sieben","acht","neun"]
decs = ["zehn", "elf", "zwölf"]
zigs = ["zwanzig" , "dreißig", "vierzig", "fünfzig", "sechszig", "siebzig", "achtzig", "neunzig"]

schreibenNummer nummer
	| nummer < 0 = "minus " ++ (schreibenNummer (-nummer))
	| nummer == 1 = "eins"
	| nummer < 10 = algs !! nummer
	| nummer < 13 = decs !! (nummer - 10)
	| nummer == 17 = "siebzehn"
	| nummer < 20 = algs !! (nummer -10) ++ "zehn"
	| nummer < 100 = schreibenNummerAux (nummer `mod` 10) ++ zigs !! ((nummer `div` 10)-2)
	| nummer < 1000 = algs !! (nummer `div` 100) ++ "hundert" ++ if nummer `mod` 100 > 0 then (schreibenNummer (nummer `mod` 100)) else ""
	| nummer < 1000000 = schreibenNummer (nummer `div` 1000) ++ "tausend" ++ if nummer `mod` 1000 > 0 then (schreibenNummer (nummer `mod` 1000)) else ""
	| otherwise = "undef"

schreibenNummerAux nummer
	| nummer == 0 = ""
	| otherwise = algs !! nummer ++ "und"
