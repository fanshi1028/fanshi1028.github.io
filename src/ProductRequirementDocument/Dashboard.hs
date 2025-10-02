{-# LANGUAGE OverloadedStrings #-}

module ProductRequirementDocument.Dashboard (dashboardPRD) where

import Data.List.NonEmpty
import ProductRequirementDocument

dashboardPRD :: ProductRequirementDocument
dashboardPRD =
  ProductRequirementDocument
    ( ProblemAlignment
        ( Problem
            "I need a dashboard for frequently needed information "
            "It is handy"
            "Some information maybe needed to be constantly updated from external public API, so could learn about Miso's fetch"
            "Showcase how to have a decent UI when the informaton density is high"
            ""
            ""
            :| []
        )
        "Actually, the dashboard should be secondary, information first and the layout adapts to it"
        ( "Information is laid out dense enough to pack information efficiently yet don't cause eyestrain and get users lost"
            :| [ "The dashboard should guide people to the information they want smoothly",
                 "Users can get the most important information just from the first scan"
               ]
        )
    )
    ( SolutionAlignment
        "Just a fixible and continuously evolving layout for a bunch of useful inforamtion"
        [ "User can expand item into details after glimping the most important information",
          "Display curent/forcasted UV Index"
        ]
        []
        []
    )
    []
