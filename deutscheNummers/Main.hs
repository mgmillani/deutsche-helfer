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

module Main where

import System.Random
import Data.IORef

import HTk.Toplevel.HTk

import Nummer

data Screen = Haupt | Lesen | Schreiben | Frei

run window Haupt = do
	lesenB <- newButton window [text "Lesen"]
	schreibenB <- newButton window [text "Schreiben"]
	freiB <- newButton window [text "Frei"]
	beendenB <- newButton window [text "Beenden"]

	pack lesenB [Side AtTop, Fill X]
	pack schreibenB [Side AtTop, Fill X]
	pack freiB [Side AtTop, Fill X]
	pack beendenB [Side AtBottom, Fill X]

	lesenCl <- clicked lesenB
	schreibenCl <- clicked schreibenB
	freiCl <- clicked freiB
	beendenCl <- clicked beendenB

	let exit = do
		destroy lesenB
		destroy schreibenB
		destroy freiB
		destroy beendenB
		done

	let lesenAction = do
		exit
		run window Lesen
		done

	let schreibenAction = do
		exit
		run window Schreiben
		done

	let freiAction = do
		exit
		run window Frei
		done

	_ <- spawnEvent (forever (
	                   (beendenCl >>> destroy window) +>
	                   (schreibenCl >>> schreibenAction) +>
	                   (freiCl >>> freiAction) +>
	                   (lesenCl >>> lesenAction)))
	finishHTk

run window Lesen = do
	zuruckB <- newButton window [text "Zurück"]
	beendenB <- newButton window [text "Beenden"]
	val <- randomRIO (0,1000)
	answer <- newIORef val

	status <- newLabel window [text "", font (Lucida,18::Int)]
	nummer <- newLabel window [text (schreibenNummer val), relief Raised]
	entry <- (newEntry window [value "", width 30])::IO (Entry String)

	pack status [Side AtTop, IPadX 10 ,PadX 10, PadY 5]
	pack nummer [Side AtTop, IPadX 10 ,PadX 10, PadY 20]
	pack entry [Side AtTop, IPadX 10 ,PadX 10, PadY 10]

	pack beendenB [Side AtBottom, Fill X]
	pack zuruckB [Side AtBottom, Fill X]

	(entered,_) <- bind entry [WishEvent [] (KeyPress (Just (KeySym "Return")))]
	zuruckCl <- clicked zuruckB
	beendenCl <- clicked beendenB

	let exit = do
		destroy status
		destroy nummer
		destroy entry
		destroy zuruckB
		destroy beendenB
		done

	let enterAction = do
		attempt <- (getValue entry)::IO String
		correct <- readIORef answer
		if attempt == show correct then do
			val <- randomRIO (0,1000)
			_ <- writeIORef answer val
			nummer # text (schreibenNummer val)
			status # text "Richtig"
			status # foreground (0::Int,130::Int,0::Int)
		else do
			status # text "Falsch"
			status # foreground (170::Int,10::Int,0::Int)
		entry # value ""
		done

	let zuruckAction = do
		exit
		run window Haupt
		done
	_ <- spawnEvent (forever (
	                   (beendenCl >>> destroy window) +>
	                   (entered >>> enterAction) +>
	                   (zuruckCl >>> zuruckAction)) )
	finishHTk

run window Schreiben = do
	zuruckB <- newButton window [text "Zurück"]
	beendenB <- newButton window [text "Beenden"]
	val <- randomRIO (0,1000)
	answer <- newIORef (schreibenNummer val)

	status <- newLabel window [text "", font (Lucida,18::Int)]
	nummer <- newLabel window [text (show val), relief Raised]
	entry <- (newEntry window [value "", width 30])::IO (Entry String)

	pack status [Side AtTop, IPadX 10 ,PadX 10, PadY 5]
	pack nummer [Side AtTop, IPadX 10 ,PadX 10, PadY 20]
	pack entry [Side AtTop, IPadX 10 ,PadX 10, PadY 10]

	pack beendenB [Side AtBottom, Fill X]
	pack zuruckB [Side AtBottom, Fill X]

	(entered,_) <- bind entry [WishEvent [] (KeyPress (Just (KeySym "Return")))]
	zuruckCl <- clicked zuruckB
	beendenCl <- clicked beendenB

	let exit = do
		destroy status
		destroy nummer
		destroy entry
		destroy zuruckB
		destroy beendenB
		done

	let enterAction = do
		attempt <- (getValue entry)::IO String
		correct <- readIORef answer
		if attempt == correct then do
			val <- randomRIO (0,1000)
			_ <- writeIORef answer (schreibenNummer val)
			nummer # text (show val)
			status # text "Richtig"
			status # foreground (0::Int,130::Int,0::Int)
		else do
			status # text "Falsch"
			status # foreground (170::Int,10::Int,0::Int)
		entry # value ""
		done

	let zuruckAction = do
		exit
		run window Haupt
		done
	_ <- spawnEvent (forever (
	                   (beendenCl >>> destroy window) +>
	                   (entered >>> enterAction) +>
	                   (zuruckCl >>> zuruckAction)) )
	finishHTk

run window Frei = do
	zuruckB <- newButton window [text "Zurück"]
	beendenB <- newButton window [text "Beenden"]

	nummer <- newMessage window [text "", aspect 1000 ,relief Raised]
	entry <- (newEntry window [value "", width 30])::IO (Entry String)

	pack nummer [Side AtTop, IPadX 10 ,PadX 10, PadY 5]
	pack entry [Side AtTop, IPadX 10 ,PadX 10, PadY 10]

	pack beendenB [Side AtBottom, Fill X]
	pack zuruckB [Side AtBottom, Fill X]

	(entered,_) <- bind entry [WishEvent [] (KeyPress (Just (KeySym "Return")))]
	zuruckCl <- clicked zuruckB
	beendenCl <- clicked beendenB

	let exit = do
		destroy nummer
		destroy entry
		destroy zuruckB
		destroy beendenB
		done

	let enterAction = do
		attempt <- (getValue entry)::IO String
		nummer # text (schreibenNummer (read attempt::Int))
		entry # value ""
		done

	let zuruckAction = do
		exit
		run window Haupt
		done
	_ <- spawnEvent (forever (
	                   (beendenCl >>> destroy window) +>
	                   (entered >>> enterAction) +>
	                   (zuruckCl >>> zuruckAction)) )
	finishHTk

 -- run _ _ = return ()

main = do
	window <- initHTk [text "Nummers auf Deutsch", size (500,200)]
	run window Haupt