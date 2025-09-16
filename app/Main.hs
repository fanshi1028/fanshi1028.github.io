{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.FileEmbed (embedFileRelative)
import Data.List.NonEmpty
import GHC.Generics
import Miso
import Miso.Html.Element
import Miso.Router
import Network.URI.Static
import qualified Pomodoro
import ProductRequirementDocument as PRD
import Prelude hiding (rem, unlines)

-----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------

main :: IO ()
main = run $ miso $ \case
  URI "" "" _ -> app
  _ -> component () noop $ \() -> p_ [] ["TEMP FIXME 404"]

data Route = Index | Pomodoro
  deriving stock (Generic)
  deriving anyclass (Router)

app :: App () action
app =
  ( component () noop $ \() ->
      div_ [] $
        [ div_ [key_ @MisoString "prd"]
            +> ( component
                   (PRD.Model False sitePRD)
                   PRD.updateModel
                   PRD.viewModel
               )
              { initialAction = Just PRD.Subscribe
              },
          div_ [key_ @MisoString "pomodoro"] +> Pomodoro.pomodoroComponent
        ]
  )
    { events = defaultEvents,
      styles = [Style $ ms $(embedFileRelative "static/output.css")]
    }

sitePRD :: ProductRequirementDocument
sitePRD =
  ProductRequirementDocument
    ( ProblemAlignment
        ( Problem
            "I need a personal site"
            "As a programmer, I need a stronger online presence, to host my blog if I have anything to say, to host my profolio, etc"
            "As an introvert, I am no good at marketing, this site and the online presence it built will stay and evolve and open door for opportunity that I may not forseen"
            "Also the blog itself is showcasing all my programming skills from Frontend to Backend even other skills like Product Design, etc"
            "It will host some tools that I will use, other could use, which potentially draw traffic."
            "A playground to play with idea that interested me"
            :| []
        )
        "The site will be built from features, one by one, and UI will come naturally. Features will be organised by tags, let's say our entry point will be a word cloud of tag? A collection of tools, blogpost, organised by tags, visitors guided by the flexible UI."
        ( "Host tools that I or visitors could use"
            :| ["UI should guide visitors what interested them smoothly", "UI should be simple, modern and not over engineered", "The site will be a testimonial of my skills and also my product sense and development philosophy"]
        )
    )
    ( SolutionAlignment
        "WorldCloudOfTag -> Features/Tools/BlogPost"
        [ "Tag Word Cloud navigation, users could click on their interested tag on the word cloud to the corresponding content",
          "Modular design, enable me to increasementally add new features with ease",
          "Every new feature will be started with a PRD(Product Requirement Document), and the PRDs will be avaliable to users, users could see the PRD of a feature on this site if they want and switch back to the feature as they wish"
        ]
        [ OpenIssues "Tech Stack" $
            "Use Miso and Tailwind, as I want to explore Miso, which makes powered by Web-Assembly(compiled from Haskell of course), later may turn it into apps via miso-lynx. tailwind is no brainer"
              :| [ "The other choice could be a haskell backend + htmx, but github page is static and no backend",
                   "Also Hyperbole(haskell lib) is also interesting, but it is not stable and again need a backend, not suitable for the use case here"
                 ],
          OpenIssues "Edge Computing, Worker" $ "Will expand into this territory and a necessary features of this site demands a backend, but not for now" :| []
        ]
        [ Reference (Just "Refactoring UI") [uri|https://www.refactoringui.com/|] $ "Following the ui philosophy in this book. feature first, then build the ui to suit the feature" :| [],
          Reference (Just "Figma PRD") [uri|https://coda.io/@yuhki/figmas-approach-to-product-requirement-docs|] $ "I like this approach to writie product requirement docs" :| []
        ]
    )
    []
