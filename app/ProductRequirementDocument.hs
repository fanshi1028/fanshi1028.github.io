{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- NOTE: reference - https://coda.io/@yuhki/figmas-approach-to-product-requirement-docs
-- NOTE: reference - https://www.cycle.app/blog/how-figma-writes-product-requirements-document-prd

module ProductRequirementDocument where

import Data.List.NonEmpty
import Data.Maybe (fromMaybe)
import Data.Text
import Data.Time
import Miso hiding (URI)
import Miso.Html.Element
import Miso.Html.Property
import Network.URI

data ProductRequirementDocument = ProductRequirementDocument
  { -- NOTE: clearly articulates the key problem we want to solve
    _problemAlignment :: ProblemAlignment,
    -- NOTE: the proposed solution and scoping features
    _solutionAlignment :: SolutionAlignment,
    -- NOTE: outlines dependencies for launch
    _launchReadiness :: [KeyMilestone]
  }
  deriving (Eq)

data SolutionAlignment = SolutionAlignment
  { _userFlows :: Text,
    _features :: [Text],
    _openIssues :: [OpenIssues],
    _references :: [Reference]
  }
  deriving (Eq)

data OpenIssues = OpenIssues
  { _openIssuesDescription :: Text,
    _keyDecisions :: NonEmpty Text
  }
  deriving (Eq)

data Reference = Reference
  { _referenceName :: Maybe Text,
    _referencelink :: URI,
    _referenceComments :: NonEmpty Text
  }
  deriving (Eq)

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
  deriving (Eq)

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
  deriving (Eq)

data KeyMilestone = KeyMilestone
  { _milestoneName :: Text,
    -- NOTE:  Make sure you answer all of the questions below and involve necessary stakeholders.
    -- a checklist of considerations you want all teams to make before actually launching a feature
    _launchChecklist :: [Text],
    _deadLine :: Maybe UTCTime
  }
  deriving (Eq)

data Model = Model
  { _open :: Bool,
    _prd :: ProductRequirementDocument
  }
  deriving (Eq)

data Action = Open -- TEMP FIXME

updateModel :: Action -> Effect parent Model Action
updateModel = noop -- TEMP FIXME

viewModel :: Model -> View Model Action
viewModel (Model False _) = div_ [] ["TEMP FIXME no PRD"]
viewModel (Model True prd) =
  div_ [class_ "flex flex-col gap-12 container mx-auto p-6 sm:p-12 bg-neutral-100"] $
    [ div_
        [class_ "flex flex-col gap-12"]
        [problemAlignmentView, solutionAlignmentView],
      launchReadinessView
    ]
  where
    h3Cls = "font-bold text-lg sm:text-xl text-neutral-400 font-serif"
    sectionView title inner =
      div_ [class_ "flex flex-col gap-8"] $
        [ h2_ [class_ "font-bold text-2xl text-neutral-400 font-serif"] [title],
          div_ [class_ "flex flex-col gap-8"] inner
        ]
    problemAlignmentView =
      sectionView "Problem Aligment" $
        [ div_ [] $
            let problem :| restProblems = prd._problemAlignment._problems
                (problemHtmlTag, title) = case restProblems of
                  [] -> (div_, "Problem")
                  _ -> (li_, "Problems")
                problemView problem' =
                  problemHtmlTag [] $
                    [h4_ [class_ "font-bold text-2xl sm:text-4xl text-neutral-600"] [text $ ms $ (problem' :: Problem)._problemStatement]]
             in [ h3_ [class_ "sr-only"] [title],
                  case restProblems of
                    [] -> problemView problem
                    _ -> ol_ [class_ "flex flex-col gap-3"] $ problemView problem : (problemView <$> restProblems)
                ],
          div_ [class_ "flex flex-col gap-2"] $
            [ h3_ [class_ h3Cls] ["High Level Approach"],
              p_ [class_ "text-neutral-800 prose"] [text $ ms prd._problemAlignment._highLevelApproach]
            ],
          div_ [class_ "flex flex-col gap-2"] $
            [ h3_ [class_ h3Cls] ["Goal and Success"],
              let goal :| restGoals = prd._problemAlignment._goalsAndSuccess
               in case restGoals of
                    [] -> p_ [class_ "text-neutral-800"] [text $ ms goal]
                    _ ->
                      let viewGoal goal' = li_ [class_ "text-neutral-800 prose"] [text $ ms goal']
                       in ol_ [class_ "list-inside list-disc flex flex-col gap-2"] $ viewGoal goal : (viewGoal <$> restGoals)
            ]
        ]
    solutionAlignmentView =
      sectionView "Solution Aligment" $
        [ div_ [class_ "flex flex-col gap-2"] $
            [ h3_ [class_ h3Cls] ["User Flows"],
              p_ [class_ "text-neutral-800 prose"] [text $ ms prd._solutionAlignment._userFlows]
            ],
          div_ [class_ "flex flex-col gap-2"] $
            [ h3_ [class_ h3Cls] ["Features"],
              case prd._solutionAlignment._features of
                [] -> p_ [] ["I am sorry, but for some reasons, this product has 0 feature surprisingly"]
                features ->
                  ul_ [class_ "list-inside list-disc flex flex-col gap-2"] $
                    let featureView feature = li_ [class_ "prose text-neutral-800"] [text $ ms feature]
                     in featureView <$> features
            ],
          div_ [class_ "flex flex-col gap-3"] $
            [ h3_ [class_ h3Cls] ["Open Issues"],
              case prd._solutionAlignment._openIssues of
                [] -> p_ [class_ "prose text-neutral-800"] ["Here is a somewhat great news, we have no open issues, but on the other hand, it could also be a indication that the PRD writer didn't think it through though"]
                openIssues ->
                  ol_ [class_ "flex flex-col gap-3 list-decimal list-inside"] $
                    let issueView (OpenIssues issueDescription (decision :| restDecisions)) =
                          li_ [class_ "marker:text-neutral-600 marker:font-semibold"] $
                            [ h4_ [class_ "inline font-bold text-neutral-600 sm:text-lg"] [text $ ms issueDescription],
                              div_ [class_ "px-2 flex flex-col sm:flex-row gap-2 mt-2"] $
                                [ h4_ [class_ "text-neutral-300 font-bold sm:text-lg sm:mt-2 sm:-ml-4 sm:[writing-mode:vertical-lr]"] ["Key Decisions"],
                                  ul_ [class_ "list-disc list-inside flex flex-col gap-2"] $
                                    let decisionView decision' = li_ [class_ "prose text-neutral-800"] [text $ ms decision']
                                     in decisionView decision : (decisionView <$> restDecisions)
                                ]
                            ]
                     in issueView <$> openIssues
            ],
          div_ [class_ "flex flex-col gap-3"] $
            [ h3_ [class_ h3Cls] ["Reference"],
              case prd._solutionAlignment._references of
                [] -> "No Reference is needed"
                references ->
                  let referenceView (Reference mName (ms . Prelude.show -> uri) (comment :| restComments)) =
                        li_ [class_ "marker:text-neutral-600 marker:font-semibold"] $
                          [ a_ [href_ uri, class_ "text-neutral-600 font-bold inline sm:text-lg"] [text $ fromMaybe uri (ms <$> mName)],
                            case restComments of
                              [] -> p_ [class_ "prose text-neutral-800"] [text $ ms comment]
                              _ ->
                                ul_ [class_ "flex flex-col list-disc list-inside"] $
                                  let commentView comment' = li_ [class_ "prose text-neutral-800"] [text $ ms comment']
                                   in commentView comment : (commentView <$> restComments)
                          ]
                   in ol_ [class_ "flex flex-col gap-3 list-decimal list-inside"] $ referenceView <$> references
            ]
        ]
    launchReadinessView =
      sectionView "Launch Readiness" $
        case prd._launchReadiness of
          [] -> [p_ [class_ "prose text-neutral-800"] ["No dependencies for launch"]]
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
