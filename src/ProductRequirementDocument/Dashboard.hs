{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module ProductRequirementDocument.Dashboard (dashboardPRD) where

import Data.List.NonEmpty
import Network.URI.Static
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
        "Just a fixable and continuously evolving layout for a bunch of useful information" --
        [ "User can expand item into details after glimping the most important information",
          "Display curent/forecasted UV Index"
        ]
        [ OpenIssues "Find some UV API to use" $
            "At first I just found a random free api: https://currentuvindex.com but 1. its api use latitude and longitude but no city option to specify location which is not good for caching (and it has daily call limit). 2. and it works globally and I base in HK mostly currently"
              :| [ "So I found one that specialise to HK: https://www.hko.gov.hk/en/weatherAPI/doc/files/HKO_Open_Data_API_Documentation.pdf",
                   "Also we have https://dragon.best/api/glax_weather/ which support specifying city"
                 ],
          OpenIssues "Efficient API call/cache" $ "" :| ["We will use haxl to solve this!"]
        ]
        [ Reference (Just "CurrentUVIndex") [uri|https://currentuvindex.com|] $ "Just a free UV Index API" :| [],
          Reference (Just "HKO Open Data API Doc (Weather API)") [uri|https://www.hko.gov.hk/en/weatherAPI/doc/files/HKO_Open_Data_API_Documentation.pdf|] $ "" :| [],
          Reference (Just "About Haxl") [uri|https://engineering.fb.com/2014/06/10/web/open-sourcing-haxl-a-library-for-haskell/|] $ "" :| []
        ]
    )
    []
