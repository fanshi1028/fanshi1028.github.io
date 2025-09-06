-- NOTE: reference - https://coda.io/@yuhki/figmas-approach-to-product-requirement-docs
-- NOTE: reference - https://www.cycle.app/blog/how-figma-writes-product-requirements-document-prd

module ProductRequirementDocument where

import Data.Text
import Data.Time
import Network.URI

data ProductRequirementDocument = ProductRequirementDocument
  { -- NOTE: clearly articulates the key problem we want to solve
    _problemAlignment :: ProblemAlignment,
    -- NOTE: the proposed solution and scoping features
    _solutionAlignment :: SolutionAlignment,
    -- NOTE: outlines dependencies for launch
    _launchReadiness :: [KeyMilestone]
  }

data SolutionAlignment = SolutionAlignment
  { _userFlows :: Text,
    _features :: [Text],
    _openIssues :: [OpenIssues],
    _references :: [Reference]
  }

data OpenIssues = OpenIssues
  { _openIssuesDescription :: Text,
    _keyDecisions :: [Text]
  }

data Reference = Reference
  { _referencelink :: URI,
    _referenceComments :: [Text]
  }

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

data ProblemAlignment = ProblemAlignment
  { _problems :: [Problem],
    -- NOTE: Optimize for eliciting a meaningful reaction.
    -- Describe briefly the approach you’re taking to solve this problem.
    -- This should be enough for the reader to imagine possible solution directions and get a very rough sense of the scope of this project.
    --  (e.g., if “The Problem” was engagement in the design process from non-designers, “The Approach” might be a feed which surfaces work that's relevant to them.)
    _highLevelApproach :: Text,
    -- NOTE: State all your goals, even those immeasurable.
    _goalsAndSuccess :: [Text]
  }

data KeyMilestone = KeyMilestone
  { _milestoneName :: Text,
    -- NOTE:  Make sure you answer all of the questions below and involve necessary stakeholders.
    -- a checklist of considerations you want all teams to make before actually launching a feature
    _launchChecklist :: [Text],
    _deadLine :: Maybe UTCTime
  }
