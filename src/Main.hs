{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Graphics.UI.FormsCompose
import Control.Applicative (liftA2)
import Text.Printf (printf)
import qualified Reactive.Banana as React
import qualified Graphics.UI.Gtk as Gtk

main :: IO ()
main = gtkFormRunModal sumDialog >>= print

-- | A form that displays a simple summation widget.
sumDialog :: GtkForm (Maybe Int)
sumDialog = gtkDialog $ do
    (stackLayout Vertical) `layoutApply` do
        x <- widgetNetwork (stackLayout Horizontal `layoutApply` (widgetNetwork sumWidget))
        y <- widgetNetwork (stackLayout Horizontal `layoutApply` (widgetNetwork sumWidget))
        let result = liftA2 (+) x y
        widgetNetwork $ gtkLabel Gtk.JustifyLeft ""
        widgetNetwork (stackLayout Horizontal `layoutApply` do
            widgetNetwork $ gtkLabel Gtk.JustifyLeft "Result: "
            widgetNetwork $ gtkReactLabel 2 Gtk.JustifyRight $ fmap (printf "%2d ") result)
        return result

-- | Simple summation widget.
sumWidget :: Widget (React.Discrete Int)
sumWidget = stackLayout Horizontal `layoutApply` do
    widgetNetwork $ gtkLabel Gtk.JustifyLeft "Sum: "
    result <- do
        a <- gtkSpinner 0 10
        widgetNetwork $ gtkLabel Gtk.JustifyCenter " + "
        b <- gtkSpinner 0 10
        widgetNetwork $ gtkLabel Gtk.JustifyCenter " = "
        return $ liftA2 (+) a b
    widgetNetwork $ gtkReactLabel 2 Gtk.JustifyRight $ fmap (printf "%2d ") result
    return result
