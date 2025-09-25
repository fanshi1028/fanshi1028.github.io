{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- NOTE: reference - https://coda.io/@yuhki/figmas-approach-to-product-requirement-docs
-- NOTE: reference - https://www.cycle.app/blog/how-figma-writes-product-requirements-document-prd

module ProductRequirementDocument
  ( -- NOTE: Types
    ProductRequirementDocument (ProductRequirementDocument),
    ProblemAlignment (ProblemAlignment),
    Problem (Problem),
    SolutionAlignment (SolutionAlignment),
    OpenIssues (OpenIssues),
    Reference (Reference),
    -- NOTE: View
    prdView,
  )
where

import Data.Aeson
import Data.List.NonEmpty
import Data.Maybe (fromMaybe)
import Data.Text
import Data.Time
import GHC.Generics
import Miso hiding (URI)
import Miso.Html.Element
import Miso.Html.Event
import Miso.Html.Property
import Miso.Svg.Element
import Miso.Svg.Property hiding (path_)
import Network.URI

data ProductRequirementDocument = ProductRequirementDocument
  { -- NOTE: clearly articulates the key problem we want to solve
    _problemAlignment :: ProblemAlignment,
    -- NOTE: the proposed solution and scoping features
    _solutionAlignment :: SolutionAlignment,
    -- NOTE: outlines dependencies for launch
    _launchReadiness :: [KeyMilestone]
  }
  deriving (Eq, Generic, FromJSON, ToJSON)

data SolutionAlignment = SolutionAlignment
  { _userFlows :: Text,
    _features :: [Text],
    _openIssues :: [OpenIssues],
    _references :: [Reference]
  }
  deriving (Eq, Generic, FromJSON, ToJSON)

data OpenIssues = OpenIssues
  { _openIssuesDescription :: Text,
    _keyDecisions :: NonEmpty Text
  }
  deriving (Eq, Generic, FromJSON, ToJSON)

data Reference = Reference
  { _referenceName :: Maybe Text,
    _referencelink :: URI,
    _referenceComments :: NonEmpty Text
  }
  deriving (Eq, Generic, FromJSON, ToJSON)

-- NOTE: Describe the problem (or opportunity) you’re trying to solve.
-- Why is it important to our users and our business?
-- What insights are you operating on?
-- And if relevant, what problems are you not intending to solve?
data Problem = Problem
  { _problemStatement :: Text,
    -- NOTE: Ask “why” one more time than you think you need, 5 times!
    _why1 :: Text,
    _why2 :: Text,
    _why3 :: Text,
    _why4 :: Text,
    _why5 :: Text
  }
  deriving (Eq, Generic, FromJSON, ToJSON)

data ProblemAlignment = ProblemAlignment
  { _problems :: NonEmpty Problem,
    -- NOTE: Optimize for eliciting a meaningful reaction.
    -- Describe briefly the approach you’re taking to solve this problem.
    -- This should be enough for the reader to imagine possible solution directions and get a very rough sense of the scope of this project.
    --  (e.g., if “The Problem” was engagement in the design process from non-designers, “The Approach” might be a feed which surfaces work that's relevant to them.)
    _highLevelApproach :: Text,
    -- NOTE: State all your goals, even those immeasurable.
    _goalsAndSuccess :: NonEmpty Text
  }
  deriving (Eq, Generic, FromJSON, ToJSON)

data KeyMilestone = KeyMilestone
  { _milestoneName :: Text,
    -- NOTE:  Make sure you answer all of the questions below and involve necessary stakeholders.
    -- a checklist of considerations you want all teams to make before actually launching a feature
    _launchChecklist :: [Text],
    _deadLine :: Maybe UTCTime
  }
  deriving (Eq, Generic, FromJSON, ToJSON)

prdView :: Bool -> ProductRequirementDocument -> View model action
prdView open prd =
  div_
    [class_ $ if open then "fixed left-0 top-0 h-full w-full overflow-auto z-40" else "hidden"]
    [ div_ [class_ "flex flex-col gap-12 md:gap-20 lg:gap-24 xl:gap-28 container mx-auto p-6 sm:p-12 md:p-16 lg:p-20 xl:p-24 2xl:p-28 bg-neutral-100 relative"] $
        [ -- button_ [onClick Close, class_ "sm:m-12 md:m-16 lg:m-20 xl:m-24 2xl:m-28 hover:animate-wiggle"]
          problemAlignmentView,
          solutionAlignmentView,
          launchReadinessView
        ]
    ]
  where
    h3Cls = "font-bold text-lg sm:text-xl md:text-2xl lg:text-3xl xl:text-4xl 2xl:text-5xl text-neutral-400 font-serif"
    sectionView title extraCls inner =
      div_ [class_ "flex flex-col gap-8 md:gap-10"] $
        [ h2_ [class_ "font-bold text-2xl md:text-3xl lg:text-4xl xl:text-5xl 2xl:text-6xl text-neutral-400 font-serif"] [title],
          div_ [classes_ ["flex flex-col gap-8 md:gap-10 lg:gap-12", extraCls]] inner
        ]
    problemAlignmentView =
      sectionView "Problem Aligment" "xl:gap-20 2xl:gap-24" $
        [ div_ [] $
            let problem :| restProblems = prd._problemAlignment._problems
                (problemHtmlTag, title) = case restProblems of
                  [] -> (div_, "Problem")
                  _ -> (li_, "Problems")
                problemView problem' =
                  problemHtmlTag [] $
                    [h4_ [class_ "font-bold text-2xl sm:text-4xl md:text-5xl lg:text-6xl xl:text-7xl 2xl:text-8xl text-neutral-600"] [text $ ms $ (problem' :: Problem)._problemStatement]]
             in [ h3_ [class_ "sr-only"] [title],
                  case restProblems of
                    [] -> problemView problem
                    _ -> ol_ [class_ "flex flex-col gap-3"] $ problemView problem : (problemView <$> restProblems)
                ],
          div_ [class_ "contents xl:block xl:relative xl:translate-y-1/2"] $
            [ svg_
                [class_ "fill-neutral-200 absolute -top-1/2 left-0 h-full hidden xl:block", xmlns_ "http://www.w3.org/2000/svg", viewBox_ "0 0 256 256", transform_ "scale(1.5,1)"]
                [ path_ [d_ "M132.93848,231.39062A8,8,0,0,1,128,224V184H48a16.01833,16.01833,0,0,1-16-16V88A16.01833,16.01833,0,0,1,48,72h80V32a8.00065,8.00065,0,0,1,13.65723-5.65723l96,96a8.003,8.003,0,0,1,0,11.31446l-96,96A8.002,8.002,0,0,1,132.93848,231.39062Z"]
                ],
              div_
                [class_ "flex flex-col xl:flex-row xl:items-center xl:justify-between gap-2 lg:gap-4 xl:-translate-y-1/2 xl:min-h-48"]
                [ h3_ [classes_ [h3Cls, "2xl:text-nowrap"]] ["High Level Approach"],
                  p_ [class_ "text-neutral-800 prose 2xl:prose-lg"] [text $ ms prd._problemAlignment._highLevelApproach]
                ]
            ],
          div_ [class_ "contents xl:block xl:relative xl:translate-y-1/2"] $
            [ svg_
                [class_ "fill-neutral-200 absolute -top-1/2 right-0 size-48 h-full hidden xl:block", xmlns_ "http://www.w3.org/2000/svg", viewBox_ "0 0 256 256", transform_ "rotate(180) scale(1.5,1)"]
                [ path_ [d_ "M132.93848,231.39062A8,8,0,0,1,128,224V184H48a16.01833,16.01833,0,0,1-16-16V88A16.01833,16.01833,0,0,1,48,72h80V32a8.00065,8.00065,0,0,1,13.65723-5.65723l96,96a8.003,8.003,0,0,1,0,11.31446l-96,96A8.002,8.002,0,0,1,132.93848,231.39062Z"]
                ],
              div_
                [class_ "flex flex-col xl:flex-row-reverse xl:items-center xl:justify-between gap-2 lg:gap-4 xl:-translate-y-1/2 xl:min-h-48"]
                [ h3_ [class_ h3Cls] ["Goal and Success"],
                  let goal :| restGoals = prd._problemAlignment._goalsAndSuccess
                   in case restGoals of
                        [] -> p_ [class_ "text-neutral-800"] [text $ ms goal]
                        _ ->
                          let viewGoal goal' = li_ [class_ "text-neutral-800 prose 2xl:prose-lg"] [text $ ms goal']
                           in ol_ [class_ "list-inside list-disc flex flex-col gap-2"] $ viewGoal goal : (viewGoal <$> restGoals)
                ]
            ]
        ]
    solutionAlignmentView =
      sectionView "Solution Aligment" "xl:flex-row xl:gap-16 xl:mt-8 2xl:mt-12" $
        [ div_
            [class_ "contents xl:flex xl:flex-col xl:gap-12 2xl:gap-16"]
            [ div_ [class_ "flex flex-col gap-2 lg:gap-4 xl:gap-6"] $
                [ h3_ [class_ h3Cls] ["User Flows"],
                  p_ [class_ "text-neutral-800 prose 2xl:prose-lg"] [text $ ms prd._solutionAlignment._userFlows]
                ],
              div_ [class_ "flex flex-col gap-2 lg:gap-4 xl:gap-6"] $
                [ h3_ [class_ h3Cls] ["Features"],
                  case prd._solutionAlignment._features of
                    [] -> p_ [] ["I am sorry, but for some reasons, this product has 0 feature surprisingly"]
                    features ->
                      ul_ [class_ "list-inside list-disc flex flex-col gap-2"] $
                        let featureView feature = li_ [class_ "prose 2xl:prose-lg text-neutral-800"] [text $ ms feature]
                         in featureView <$> features
                ],
              div_ [class_ "flex flex-col gap-3 lg:gap-4 xl:gap-6 order-last"] $
                [ h3_ [class_ h3Cls] ["Reference"],
                  case prd._solutionAlignment._references of
                    [] -> "No Reference is needed"
                    references ->
                      let referenceView (Reference mName (ms . Prelude.show -> uri) (comment :| restComments)) =
                            li_ [class_ "marker:text-neutral-600 marker:font-semibold sm:text-lg md:text-xl lg:text-2xl xl:text-3xl 2xl:text-4xl"] $
                              [ div_
                                  [class_ "contents lg:inline-flex xl:contents lg:flex-row lg:gap-4"]
                                  [ a_ [href_ uri, class_ "text-neutral-600 font-bold inline sm:text-lg md:text-xl lg:text-2xl xl:text-3xl 2xl:text-4xl"] [text $ fromMaybe uri (ms <$> mName)],
                                    case restComments of
                                      [] -> p_ [class_ "prose 2xl:prose-lg text-neutral-800 xl:mt-4"] [text $ ms comment]
                                      _ ->
                                        ul_ [class_ "flex flex-col list-disc list-inside xl:mt-4"] $
                                          let commentView comment' = li_ [class_ "prose 2xl:prose-lg text-neutral-800"] [text $ ms comment']
                                           in commentView comment : (commentView <$> restComments)
                                  ]
                              ]
                       in ol_ [class_ "flex flex-col gap-3 xl:gap-6 list-decimal list-inside"] $ referenceView <$> references
                ]
            ],
          div_ [class_ "flex flex-col gap-3 sm:gap-6 xl:gap-12"] $
            [ h3_ [class_ h3Cls] ["Open Issues"],
              case prd._solutionAlignment._openIssues of
                [] -> p_ [class_ "prose 2xl:prose-lg text-neutral-800"] ["Here is a somewhat great news, we have no open issues, but on the other hand, it could also be a indication that the PRD writer didn't think it through though"]
                openIssues ->
                  ol_ [class_ "flex flex-col gap-3 sm:gap-6 list-decimal list-inside"] $
                    let issueView (OpenIssues issueDescription (decision :| restDecisions)) =
                          li_ [class_ "marker:text-neutral-600 marker:font-semibold sm:text-lg md:text-xl lg:text-2xl xl:text-3xl 2xl:text-4xl"] $
                            [ h4_ [class_ "inline font-bold text-neutral-600 text-lg sm:text-xl md:text-xl lg:text-2xl xl:text-3xl 2xl:text-4xl"] [text $ ms issueDescription],
                              div_ [class_ "px-2 flex flex-col sm:flex-row xl:flex-row-reverse xl:justify-between gap-2 sm:gap-4 lg:gap-6 mt-2 sm:mt-4"] $
                                [ h4_ [class_ "text-neutral-300 font-bold sm:text-2xl md:text-3xl lg:text-4xl xl:text-5xl 2xl:text-6xl sm:mt-2 sm:-ml-4 xl:ml-0 sm:[writing-mode:vertical-lr]"] ["Key Decisions"],
                                  ul_ [class_ "list-disc list-inside flex flex-col gap-2 sm:gap-4"] $
                                    let decisionView decision' = li_ [class_ "prose 2xl:prose-lg text-neutral-800"] [text $ ms decision']
                                     in decisionView decision : (decisionView <$> restDecisions)
                                ]
                            ]
                     in issueView <$> openIssues
            ]
        ]
    launchReadinessView =
      sectionView "Launch Readiness" "" $
        case prd._launchReadiness of
          [] -> [p_ [class_ "prose 2xl:prose-lg text-neutral-800"] ["No dependencies for launch"]]
          _ ->
            let keyMilestoneView (KeyMilestone name checkList deadline) =
                  li_
                    []
                    [ h3_ [class_ h3Cls] [text $ ms name],
                      case checkList of
                        [] -> "Nothing to check for the milestone"
                        _ -> ol_ [] $ li_ [] . (: []) . text . ms <$> checkList,
                      "TEMP FIXME: deadline"
                    ]
             in keyMilestoneView <$> prd._launchReadiness
