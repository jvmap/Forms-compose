{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, RankNTypes, CPP #-}

module Graphics.UI.FormsCompose (
    GtkForm(..),
    Widget (..),
    widgetNetwork,
    WidgetNetwork(..),
    widgetAddToBin,
    Layout (..),
    gtkWindow,
    gtkWidgetWindow,
    gtkMessageBox,
    gtkDialog,
    gtkLabel,
    gtkReactLabel,
    gtkSpinner,
    gtkButton,
    stackLayout,
    gtkButtonLayout,
    gtkButtonLayoutWithSecondary,
    Orientation (..),
    -- Alignment,
    HorizontalAlignment (HALeft, HACenter, HARight),
    VerticalAlignment (VABottom, VACenter, VATop),
    TextAlignment,
) where

import qualified Graphics.UI.Gtk as Gtk
import Control.Applicative (liftA2)
import qualified Reactive.Banana as React
import Control.Monad.IO.Class (liftIO)
import Data.IORef (newIORef, writeIORef, readIORef)
import Graphics.Rendering.Pango (EllipsizeMode(..))

data GtkForm a = GtkForm {
    -- | The IO action that displays the form and yields its result (blocking).
   gtkFormRunModal :: IO a
}

gtkWindow :: a -- ^ Default value to return if the window is closed using the close button.
          -> React.NetworkDescription (Gtk.Window, React.Event a) -- ^ The window to show. The result 'React.Event' closes the window.
          -> GtkForm a
gtkWindow dft innerNetwork = GtkForm {
    gtkFormRunModal = do
        result <- newIORef dft
        Gtk.initGUI
        network <- React.compile $ do
            (window, theResult) <- innerNetwork
            evImplicitClose <- fmap (fmap (const dft)) (_windowCloseEvent window)
            liftIO $ do
                Gtk.widgetShowAll window
                -- For some reason, the default widget is not taken care of correctly by GTK.
                -- Reset it after the window is shown.
                Gtk.windowGetDefaultWidget window >>= Gtk.windowSetDefault window
            React.reactimate $ fmap (\x -> do
                writeIORef result x
                Gtk.mainQuit) (theResult `React.union` evImplicitClose)
        React.actuate network
        Gtk.mainGUI
        readIORef result
}

gtkWidgetWindow :: a -- ^ Default value to return if the window is closed using the close button.
          -> Widget (React.Event a) -- ^ Composite widget to show. The result 'React.Event' closes the window.
                                    -- Its value is used as the result value of the form.
          -> GtkForm a
gtkWidgetWindow dft widget = gtkWindow dft $ do
    window <- liftIO $ do
        window <- Gtk.windowNew
        Gtk.windowSetPosition window Gtk.WinPosCenter
        Gtk.containerSetBorderWidth window 8
        return window
    evExplicitClose <- widgetAddToBin window widget
    return (window, evExplicitClose)

_windowCloseEvent :: Gtk.Window -> React.NetworkDescription (React.Event ())
_windowCloseEvent window = React.fromAddHandler add where 
    add :: React.AddHandler ()
    add handler = do
        cid <- window `Gtk.on` Gtk.deleteEvent $ Gtk.tryEvent (liftIO (handler ()))
        return (Gtk.signalDisconnect cid)

-- | Turns a 'Widget' producing 'Discrete' values of type @a@ into a form returning either:
--
-- * 'Just' the value of the widget at the time the /Ok/ button was pressed;
-- 
-- * 'Nothing'; in case the /Cancel/ button was pressed, or the dialog was closed using the close button.
gtkDialog :: Widget (React.Discrete a) -> GtkForm (Maybe a)
gtkDialog widget = gtkWidgetWindow Nothing widget' where
    widget' = stackLayout Vertical `layoutApply` do
        result <- fmap (fmap Just) (widgetNetwork widget)
        widgetNetwork $ gtkLabel Gtk.JustifyLeft ""  -- Line break
        widgetNetwork $ gtkButtonLayout Horizontal Gtk.ButtonboxEnd $ do
            cancel <- widgetNetwork $ fmap (fmap (const Nothing)) (gtkButtonStock Gtk.stockCancel False)
            ok <- widgetNetwork $ fmap (result React.<@) (gtkButtonStock Gtk.stockOk True)
            return $ React.union ok cancel

-- | A form that displays a text message and an Ok button.
gtkMessageBox :: Gtk.MessageType -> String -> GtkForm ()
gtkMessageBox msgType text = GtkForm {
    gtkFormRunModal = do
        Gtk.initGUI
        dlg <- Gtk.messageDialogNew Nothing [] msgType Gtk.ButtonsOk text
        -- Bah, does not appear on the taskbar.
        Gtk.dialogRun dlg
        return ()
}

data Widget a = Widget {
    widgetRealize :: React.NetworkDescription (Gtk.Widget, a)
}

instance Functor Widget where
    fmap f (Widget r) = Widget (fmap (\(w, x) -> (w, f x)) r)

widgetNetwork :: Widget a -> WidgetNetwork a
widgetNetwork wdg = WidgetNetwork {
    widgetNetworkRealize = \realize -> do
        (widget, result) <- widgetRealize wdg
        liftIO (realize widget)
        return result
}

-- | Embeds the given widget in a Gtk bin.
widgetAddToBin :: Gtk.BinClass c => c -> Widget a -> React.NetworkDescription a
widgetAddToBin bin wdg = do
    (backing, result) <- widgetRealize wdg
    liftIO (Gtk.containerAdd bin backing)
    return result

data Layout = Layout {
    layoutApply :: forall a. WidgetNetwork a -> Widget a
}

-- TODO: delete this data type
data WidgetNetwork a = WidgetNetwork {
    -- | Constructs all controls, registers events, etc.
    -- The first argument is an action that adds Gtk widgets to a parent container.
    widgetNetworkRealize :: (Gtk.Widget -> IO ()) -> React.NetworkDescription a
}

-- TODO: Right-To-Left, Bottom-To-Top orientations
data Orientation = Horizontal | Vertical

type Alignment = (HorizontalAlignment, VerticalAlignment)

-- | Note: when regional settings indicate right-to-left reading, 'HALeft' actually
-- aligns to the right, and 'HARight' aligns to the left.
data HorizontalAlignment = HALeft 
                         | HACenter 
                         | HARight 
                         | HAFill
                         
data VerticalAlignment = VATop 
                       | VACenter 
                       | VABottom 
                       | VAFill

-- | The 'Layout' that stacks the network widgets in the passed 'Orientation'.
stackLayout :: Orientation -> Layout
stackLayout Horizontal = Layout {
    layoutApply = _layoutInBox $ Gtk.hBoxNew False 6
}
stackLayout Vertical = Layout {
    layoutApply = _layoutInBox $ Gtk.vBoxNew False 6
}

_layoutInBox :: Gtk.BoxClass b => IO b -> WidgetNetwork a -> Widget a
_layoutInBox boxNew (WidgetNetwork innerWidgetPack) = Widget {
    widgetRealize = do
        box <- liftIO boxNew
        result <- innerWidgetPack (_addToBox Gtk.PackGrow box)
        return (Gtk.toWidget box, result)
}

-- | GTK's button box layout. Specially suited for, uh, buttons, although it can be used with any other widget as well.
-- A 'gtkButtonLayout' differs from a 'stackLayout' in the following ways:
--
-- * All widgets in a 'gtkButtonLayout' are allocated equal screen space.
-- 
-- * Widgets are allocated a minimum size.
gtkButtonLayout :: Orientation 
                -> Gtk.ButtonBoxStyle
                -> WidgetNetwork a
                -> Widget a
gtkButtonLayout orientation style prim = fmap fst $
    gtkButtonLayoutWithSecondary orientation style prim (return ())

-- | Same as 'gtkButtonLayout', but 'gtkButtonLayoutWithSecondary' supports /primary/ and /secondary/ widget networks. 
-- The primary network is allocated the most prominent portion of the button layout, while the secondary network 
-- is alloted lesser valued space. Both the primary and the secondary widget networks can contain zero or more buttons 
-- or other widgets.
gtkButtonLayoutWithSecondary :: Orientation 
                             -> Gtk.ButtonBoxStyle
                             -> WidgetNetwork a -- ^ The primary widget network.
                             -> WidgetNetwork b -- ^ The secondary widget network.
                             -> Widget (a, b)
gtkButtonLayoutWithSecondary Horizontal = _layoutInButtonBox Gtk.hButtonBoxNew
gtkButtonLayoutWithSecondary Vertical   = _layoutInButtonBox Gtk.vButtonBoxNew

_layoutInButtonBox :: Gtk.ButtonBoxClass bb => IO bb -> Gtk.ButtonBoxStyle -> WidgetNetwork a -> WidgetNetwork b -> Widget (a, b)
_layoutInButtonBox boxNew style (WidgetNetwork primPack) (WidgetNetwork secPack) = Widget {
    widgetRealize = do
        btnBox <- liftIO $ do
            btnBox <- boxNew
            Gtk.buttonBoxSetLayout btnBox style
            Gtk.boxSetSpacing btnBox 6
            return btnBox
        result <- liftA2 (,)
            (primPack (_addToBox Gtk.PackGrow btnBox))
            (secPack (_addToBoxSec btnBox))
        return (Gtk.toWidget btnBox, result)
} where
    _addToBoxSec btnBox child = do
        _addToBox Gtk.PackGrow btnBox child
        Gtk.buttonBoxSetChildSecondary btnBox child True

_addToBox :: (Gtk.BoxClass b, Gtk.WidgetClass w) => Gtk.Packing -> b -> w -> IO ()
_addToBox packing box wdg = Gtk.boxPackStart box wdg packing 0

_alignmentNew :: Alignment -> IO Gtk.Alignment
_alignmentNew (ha, va) = Gtk.alignmentNew (_xAlign ha) (_yAlign va) (_xScale ha) (_yScale va)

_xAlign HALeft   = 0
_xAlign HACenter = 0.5
_xAlign HARight  = 1
_xAlign HAFill   = 0
_xScale HALeft   = 0
_xScale HACenter = 0
_xScale HARight  = 0
_xScale HAFill   = 1
_yAlign VATop    = 0
_yAlign VACenter = 0.5
_yAlign VABottom = 1
_yAlign VAFill   = 0
_yScale VATop    = 0
_yScale VACenter = 0
_yScale VABottom = 0
_yScale VAFill   = 1


type TextAlignment = Gtk.Justification

-- | A widget containing non-interactive text.
gtkLabel :: TextAlignment -> String -> Widget ()
gtkLabel textAlignment text = Widget {
    widgetRealize = liftIO $ do
        lbl <- Gtk.labelNew (Just text)
        Gtk.labelSetLineWrap lbl True
        Gtk.labelSetJustify lbl textAlignment
        return (Gtk.toWidget lbl, ())
}

-- | A widget containing non-interactive text that reacts to changes. To prevent changes in GUI layout
-- when the text changes, a maximum number of characters must be given. The label will request space
-- for exactly that number of characters. If the input string contains more characters than can be 
-- displayed by the label, ellipsis (...) will be used.
gtkReactLabel :: Int                   -- ^ The maximum number of characters. Note: In GTK, values less than 3 are treated as 3.
              -> TextAlignment
              -> React.Discrete String -- ^ Transient text message.
              -> Widget ()
gtkReactLabel maxChars textAlignment stream = Widget {
    widgetRealize = do
        lbl <- liftIO $ do
            lbl <- Gtk.labelNew (Just (React.initial stream))
            Gtk.labelSetLineWrap lbl True
            Gtk.labelSetWidthChars lbl maxChars
            Gtk.set lbl [ Gtk.miscXalign Gtk.:= _xAlign 
                (_horizontalAlignmentFromTextAlignment textAlignment) ]
            Gtk.labelSetJustify lbl textAlignment
            Gtk.labelSetEllipsize lbl EllipsizeEnd
            return lbl
        React.reactimate (fmap (Gtk.labelSetText lbl) (React.changes stream))
        return (Gtk.toWidget lbl, ())
}

_horizontalAlignmentFromTextAlignment :: TextAlignment -> HorizontalAlignment
_horizontalAlignmentFromTextAlignment Gtk.JustifyLeft   = HALeft
_horizontalAlignmentFromTextAlignment Gtk.JustifyCenter = HACenter
_horizontalAlignmentFromTextAlignment Gtk.JustifyRight  = HARight
_horizontalAlignmentFromTextAlignment Gtk.JustifyFill   = HAFill

-- | Spinner for integer values.
gtkSpinner :: Int -- ^ Min value
           -> Int -- ^ Max value
           -> WidgetNetwork (React.Discrete Int)
gtkSpinner min max = WidgetNetwork {
    widgetNetworkRealize = \realize -> do
        sb <- liftIO $ Gtk.spinButtonNewWithRange (fromIntegral min) (fromIntegral max) (fromIntegral 1)
        liftIO $ do
            realize (Gtk.toWidget sb)
        let
            valueChanges = React.fromAddHandler add
            add handler = do
                cid <- Gtk.afterValueSpinned sb (Gtk.spinButtonGetValueAsInt sb >>= handler)
                return (Gtk.signalDisconnect cid)
        fmap (React.stepperD min) valueChanges
}

-- | A button that fires an event when clicked.
gtkButton :: String -- ^ Caption
          -> Bool   -- ^ Whether this is the default button
          -> Widget (React.Event ())
gtkButton caption isDefault = _button isDefault (Gtk.buttonNewWithMnemonic caption)

gtkButtonStock :: Gtk.StockId 
               -> Bool   -- ^ Whether this is the default button 
               -> Widget (React.Event ())
gtkButtonStock stockId isDefault = _button isDefault (Gtk.buttonNewFromStock stockId)

_button :: Bool -> IO Gtk.Button -> Widget (React.Event ())
_button isDefault constr = Widget {
    widgetRealize = do
        btn <- liftIO $ do 
            btn <- constr
            Gtk.set btn [ Gtk.widgetCanDefault Gtk.:= isDefault ]
            return btn
        React.liftIOLater $ Gtk.set btn [ Gtk.widgetHasDefault Gtk.:= isDefault ]
        let add handler = do
            cid <- btn `Gtk.on` Gtk.buttonActivated $ handler ()
            return (Gtk.signalDisconnect cid)
        result <- React.fromAddHandler add
        return (Gtk.toWidget btn, result)
}

instance Functor WidgetNetwork where
     fmap f xs = xs >>= return . f

instance Monad WidgetNetwork where
    return x = WidgetNetwork {
        widgetNetworkRealize = const (return x)
    }
    
    widgetA >>= f = WidgetNetwork {
        widgetNetworkRealize = \box -> do
            out <- widgetNetworkRealize widgetA box
            widgetNetworkRealize (f out) box
    }
