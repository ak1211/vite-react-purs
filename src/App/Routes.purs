module App.Routes
  ( Page(..)
  , ProductId
  , Route(..)
  , parseRoute
  , printRoute
  , productId
  ) where

import Prelude hiding ((/))
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Routing.Duplex (RouteDuplex', default, end, int, parse, prefix, print, root, segment, path)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Routing.Duplex.Parser (RouteError)

data Route
  = Page Page
  | NotFound

data Page
  = Home
  | About
  | Charts

derive instance Generic Route _
derive instance eqRoute:: Eq Route

derive instance Generic Page _
derive instance eqPage:: Eq Page

type ProductId = Int

productId :: RouteDuplex' Int
productId = int segment

routes :: RouteDuplex' Route
routes =
  default NotFound $
    sum
      { "Page": pages
      , "NotFound": "404" / noArgs
      }

pages :: RouteDuplex' Page
pages =
  root $ prefix "vite-react-purs" $ end $ sum
    { "Home":  "" / noArgs
    , "About": "about" / noArgs
    , "Charts": "charts" / noArgs
    }

parseRoute :: String -> Either RouteError Route
parseRoute = parse routes

printRoute :: Page -> String
printRoute = print pages