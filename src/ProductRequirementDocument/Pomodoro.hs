{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module ProductRequirementDocument.Pomodoro (pomodoroPRD) where

import Data.List.NonEmpty
import Network.URI.Static
import ProductRequirementDocument

pomodoroPRD :: ProductRequirementDocument
pomodoroPRD =
  ProductRequirementDocument
    ( ProblemAlignment
        ( Problem
            "A simple Pomodoro app"
            "I need some quick and simple on my site as a MVP to showcase my programming skills"
            "So that my site is not empty and it will be a tool that I could use"
            "Bonus It is a tool that people could use and potentially bring traffic"
            ""
            ""
            :| []
        )
        "Starting from features, evolving into a nice UI, don't overengineering it."
        ("A functional pomodoro timer" :| ["UI/UX should be simple and modern"])
    )
    ( SolutionAlignment
        "Settings -> Timer"
        [ "Users can set their desired time for pomodoro and breaks",
          "Users can start and stop the timer",
          "Users can fastforward to skip to the next stage"
        ]
        [ OpenIssues
            "Timer Settings Reasonable Values"
            $ "5 mins as the unit, I don't see we need more flexibility here, make it simple"
              :| [ "More than 5 mins, It is too fragment to do reasonable work or have a reasonable rest if the time is too short",
                   "Less than 45 mins, I don't think and human could sustain a undistrubed focus for that long while being productive",
                   "Following the tried and true 4 short break then a long break, I don't see the value of extra flexibilify as we offer to tweak the time for each stage already"
                 ]
        ]
        [Reference (Just "Pomofocus") [uri|https://pomofocus.io/|] $ "I don't need those complex features like it does but I like the simple and clean ui" :| []]
    )
    []
