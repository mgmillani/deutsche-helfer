{-
	This file is part of Wörter Genus.

    Wörter Genus is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Wörter Genus is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Wörter Genus.  If not, see <http://www.gnu.org/licenses/>.
 -}

module Main where

import System.Directory
import System.Random
import Data.IORef

import HTk.Toplevel.HTk

import Laden

windowX = 200
buttonY = 60

myLen = foldl (\x y -> x+1) 0

haupt window modules = do
	beendenB <- newButton window [text "Beenden"]
	buttons <- mapM (\x -> newButton window [text x]) modules

	window # size (windowX,buttonY*(1 + (myLen buttons)) )

	mapM_ (\x -> pack x [Side AtTop,Fill X]) buttons
	pack beendenB [Side AtBottom, Fill X]

	beendenCl <- clicked beendenB
	clicks <- mapM clicked buttons

	let exit = mapM destroy (beendenB:buttons)
	let choose filename = do
		daten <- loadData $ "daten/" ++ filename
		spiel window daten
		done

	let actions clks names =
		case (clks,names) of
			((c:[]),(n:_)) -> (c >>> (do exit ; choose n))
			((c:rc),(n:rn)) -> (c >>> (do exit ; choose n)) +> (actions rc rn)

	_ <- spawnEvent (forever (
	                   (beendenCl >>> destroy window) +>
	                   (actions clicks modules)
	                         )
	                )
	finishHTk

spiel window worter = do
	beendenB <- newButton window [text "Beenden"]
	status <- newLabel window [text "", font (Lucida,18::Int)]
	let upperIndex = length worter - 1
	pos <- randomRIO (0,upperIndex)
	wort <- newLabel window [text $ snd $ worter !! pos]
	answer <- newIORef $ fst $ worter !! pos
	genus <- newVBox window []
	derB <- newButton genus [text "der", foreground (130::Int,5::Int,0::Int)]
	dieB <- newButton genus [text "die", foreground (5::Int,130::Int,0::Int)]
	dasB <- newButton genus [text "das", foreground (0::Int,5::Int,130::Int)]
	zuruck <- newButton window [text "Züruck"]

	pack status [Side AtTop,PadY 10]
	pack beendenB [Side AtBottom, Fill X]
	pack zuruck [Side AtBottom, Fill X]
	pack derB [Side AtTop, Fill X]
	pack dieB [Side AtTop, Fill X]
	pack dasB [Side AtTop, Fill X]
	pack genus [Side AtLeft]
	pack wort [Side AtLeft]

	derCl <- clicked derB
	dieCl <- clicked dieB
	dasCl <- clicked dasB
	zuruckCl <- clicked zuruck
	beendenCl <- clicked beendenB

	let exit = do
		destroy derB
		destroy dieB
		destroy dasB
		destroy zuruck
		destroy beendenB
		destroy wort
		destroy genus
		destroy status

	let zuruckAction = do
		exit
		files <- getDirectoryContents "daten"
		let modules = (filter (\x -> x /= "." && x /= "..") files)
		haupt window modules
		done

	let check attempt = do
		right <- readIORef answer
		if right == attempt then do
			-- marca como correto
			status # text "Richtig"
			status # foreground (0::Int,135::Int,5::Int)
			-- pega uma nova palavra
			pos <- randomRIO (0,upperIndex)
			let (g,w) = worter !! pos
			writeIORef answer g
			wort # text w
		else do
			-- marca como errado
			status # text "Falsch"
			status # foreground (135::Int,5::Int,0::Int)
		done

	let derAction = check "der"
	let dieAction = check "die"
	let dasAction = check "das"

	_ <- spawnEvent (forever (
	                   (derCl >>> derAction) +>
	                   (dieCl >>> dieAction) +>
	                   (dasCl >>> dasAction) +>
	                   (zuruckCl >>> zuruckAction) +>
	                   (beendenCl >>> destroy window)
	                         )
	                )

	finishHTk

main = do
	window <- initHTk [text "Wörter Genus", size (windowX,200)]
	files <- getDirectoryContents "daten"
	let modules = (filter (\x -> x /= "." && x /= "..") files)

	haupt window modules
