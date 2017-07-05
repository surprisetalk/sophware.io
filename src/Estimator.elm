
module Estimator exposing ( Estimate, init, view )

-- TODO: at the end, we should have upfront & monthly totals (eg maintenance)

-- TODO: don't be TOO granular
-- TODO:   this is becoming a problem

-- TODO: the dollar number should "count" up and "count" down animatedly
-- TODO:   clicking should make like a video-game "ghost" effect, eg sims purchases?

-- TODO: "if we didn't accurately capture everything you wanted, send us a message and we'll give you a better estimate"

-- TODO: write sophware blog posts about these "new" markets we're trying to start

-- TODO: progress bar indicating how much they've filled out
-- TODO: or fixed bulma.menu on the left to tell you where you are + navigate

-- TODO: are you sure you want to do this? you'll lose all progress

-- TODO: at the end, give an easy/hard breakdown for all the options they picked, and realistic expectations

-- TODO: "think your project is 'too hard'? we'll do a double-or-nothing wager!"

{-

|                                    |
|        SOPHWARE ESTIMATOR          |
|                                    |
|        |    options     |          |
|        |    options     |          |
--------------------------------------
| nav    |    options     | remarks  |
| nav    |    options     |          |
| nav    |    options     | remarks  |
|        |    options     | remarks  |
| price  |    options     |          |
--------------------------------------
|        |    options     | remarks  |
|        |    options     |          |
|        |                |          |
|        |    overview    |          |
|        |    total       |          |

-- TODO: hover over things for more details?
-- TODO:   but that's not mobile-friendly

-}


-- IMPORTS ---------------------------------------------------------------------


-- HELPERS ---------------------------------------------------------------------


-- ESTIMATE --------------------------------------------------------------------

type alias Estimate
  = { project     : Maybe Project
    , impossible  : Maybe Bool
      -- TODO: "impossible" discount? haha
      -- TODO: this NEEDS to be near the top
    , urgency     : Maybe Urgency
    , nonProfit   : Maybe Nod
    , paymentPlan : Maybe PaymentPlan
    }

type Nod = No
         | Yes
         | Dunno

type Urgency = Yesterday
             | Today
             | ThisWeek
             | ThisMonth
             | ThisQuarter
             | ThisYear
             | ThisDecade
             | Never

type Scale = None
           | Low
           | Medium
           | High

type PaymentPlan = Cash
                 | Monthly
                 | Quarterly
                 | ProfitSharing


-- PROJECT ---------------------------------------------------------------------

type Project = ProjectApp         App
             | ProjectImprovement Improvement
             | ProjectBranding    Branding
             | ProjectResearch    Research
             | ProjectAutomation  Automation
             | ProjectOther


-- APP -------------------------------------------------------------------------

-- TODO: we can do presets like "blog", "store", and "portfolio"

-- TODO: they should have a clear path to "rigorous" AI-type projects

type alias App
  = { platform   : Platform
    , size       : Maybe Scale
    , quality    : Maybe Scale
    , importance : Maybe Scale
    , category   : Maybe AppCategory
    , branding   : Maybe Branding
    }

type Platform = Mobile Mobile
              | Web    Web

type AppCategory
-- TODO: each of these should have unique admin options
-- TODO: we may want to mov AppFeatures down into each alias record
  = Blog_      AppFeatures Blog
  | Store_     AppFeatures Store
  | Portfolio_ AppFeatures Portfolio
  | Service_   AppFeatures Service
  | Game_      AppFeatures
               AdvancedApp Service
  | Advanced_  AdvancedApp Service
  | Other_     AppFetaures Blog
                           Store
                           Portfolio
                           Service
                           AdvancedApp

type alias AppFeatures
  = { accounts     : Maybe AppAccounts
    , email        : Maybe AppEmail
    , search       : Maybe AppSearch
    , content      : Maybe AppContent
    , social       : Maybe AppSocial
    , commerce     : Maybe AppCommerce
    , external     : Maybe AppExternal
    , security     : Maybe Scale
    , contact      : Maybe Nod
    , management   : Maybe Nod
    , maintenance  : Maybe Nod
    , analytics    : Maybe Nod
      -- TODO: this is where we weed out the "idea" people
    }

type AppAccounts = SocialAccount
                 | NativeAccount
                 | HybridAccount
                 | NoAccount

type AppEmail = FewEmailsPerDay
              | FewEmailsPerMonth
              | FewEmailsPerYear
              | NoEmail

type AppSearch = SimpleSearch
               | AdvancedSearch
               | NoSearch
  
type alias AppContent
  = { copy     : Maybe Nod
    , images   : Maybe Nod
    , graphics : Maybe Nod
    , video    : Maybe Nod
    }

type alias AppSocial
  = { messages : Maybe Nod
    , comments : Maybe Nod
    , sharing  : Maybe Nod
    }

type AppCommerce = Subscriptions
                 | Donations
                 | Marketplace
                 | Retail
                 | OtherCommerce
                 | NoCommerce

type alias AppExternal
  = { publicAPI : Maybe Nod
    , sms       : Maybe Nod
    }

type alias AppAdvanced
  -- TODO: break these up into advanced and service
  -- TODO: give some of these a "fun" discount
  = { hardCamera      : Maybe Nod
    , imageProcessing : Maybe Nod
    , bigData         : Maybe Nod
    , cloudStorage    : Maybe Nod
    , academic        : Maybe Nod
    , textAnalysis    : Maybe Nod
    , iot             : Maybe Nod
    , parallelism     : Maybe Nod
    , metaProgramming : Maybe Nod
    }


-- APP -------------------------------------------------------------------------

type alias Branding
  = { new        : Maybe Bool  -- are they a new company?
    , logo       : Maybe Nod
    , social     : Maybe Nod
    , content    : Maybe BrandingContent
    , guide      : Maybe Nod
    , strategy   : Maybe Nod
    , marketing  : Maybe Nod
    , website    : Maybe Scale -- does your site need refreshing
    , management : Maybe Nod
    -- , vision   : True       -- "you get this one for free!"
    }

type alias BrandingContent
  = { copy    : Maybe Scale -- how much copy?
    , images  : Maybe Scale -- how many images?
    , video   : Maybe Scale -- how many videos?
    }


-- PROJECT ---------------------------------------------------------------------

type alias Improvement
  = { need          : List  ImprovementNeed
    , size          : Maybe Scale
    , quality       : Maybe Scale
    , importance    : Maybe Scale
    , connectedness : Maybe Scale
    , language      : List  Lang
    , openSource    : Maybe Nod
    }

type ImprovementNeed
  = Triage
  | CodeCleanUp
  | ImproveTesting
  | ImproveSecurity
  | ImproveDocumentation
  | ImprovePerformance
  | AddFeature
  | FixFeature
  | AddSystem
  | FixSystem
  | FullRedesign
  | PartialRedesign
  | OtherNeed
    -- TODO: at the total, "$XXX + ?" for other need
    -- TODO:   this is good because we won't be adding "guess" dollars

type Lang
-- TODO: pull icons from  codewars
  = JS
  | PHP
  | Python
  | Clojure
  | Haskell
  | Swift
  | Prolog
  | APL
  | Lisp
  | SQL
  | Elixir
  | Shell
  | C
  | OCaml
  | Elm
  | PureScript
  | OtherLang
  -- TODO: put the esoteric langs on the side
  -- TODO:   these are actually really important; make sure to 
  -- TODO: should we include other languages, so that we know what we're getting into?
  -- TODO:   we could always just charge extra for "ugly" langs
  -- TODO: add some of these to the main page
      

-- RESEARCH --------------------------------------------------------------------

-- TODO: ONE HUNDRED PERCENT SATISFACTION GUARANTEED

-- TODO: make it clear that we'll take on ANY problem

-- TODO: if we can't find an answer, you pay nothing
-- TODO: this is something we need to make a dedicated page for! this is a service that doesn't exist

-- TODO: "i want a general analysis" vs "i want to solve a problem"

type Research
  = Analysis Study
  | Problem
    -- TODO: tell them more about our problem-solving and give them the contact form

type alias Study
  = { data  : Maybe Nod   -- do they need to gather the data?
    , quant : Maybe Nod
    , study : List  StudyType
    , pres  : List  Presentation
    }

type StudyType
  = StudySoftware_   StudySoftware
  | StudyBusiness_   StudyBusiness
  | StudyData_       StudyData
  | StudyInnovation_ -- R&D
  | StudyScience_    -- other sciences (what about things like "education"?)
    -- TODO: this list needs improvement
    -- TODO: this "general" study ideas might give the wrong impression
    -- TODO:   we need some "oddly" specific examples to show what "good" studies look like
    -- TODO:     user conversion
    -- TODO:     employee productivity
    -- TODO:     onboarding speed
    -- TODO:     materials efficiency
    -- TODO:     database latency
    -- TODO:     product potential
    -- TODO:     r&d options

type StudyData
  = StudyTextSemantic
  | StudyTextSentiment
  | StudyVideo
  | StudyPhoto
  | StudyOther

type StudySoftware
  = StudyCodeQuality
  | StudyArchitecture
  | StudyPerformance
  | StudySecurity
  | StudyInterface

type StudyBusiness
  = StudyMarket
  | StudyProduct
  | StudyOperations
  | StudyFinance
  | StudyEthics

type Presentation
-- TODO: "default" is short writeup
-- TODO: make it clear that we don't do fancy academic/enterprise "speak"
-- TODO:   "we like simple words"
-- TODO:   we will not "massage" our findings to your liking
-- TODO:     we will keep findings private unless it's actively harming others
  = WhitePaper
  | Documentary
  | Spreadsheet
  | Video
  | Interactive
  | Slides
  | Article
  | Other

      
-- INIT ------------------------------------------------------------------------


-- VIEW ------------------------------------------------------------------------


