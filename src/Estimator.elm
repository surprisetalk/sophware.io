
module Estimator exposing ( Estimator, init, view )

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

-- TODO: where would UX consulting fit in here?

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

import Html exposing (..)


-- HELPERS ---------------------------------------------------------------------


-- ESTIMATOR -------------------------------------------------------------------

type alias Estimator
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
             | ProjectAutomation  
             | ProjectOther


-- APP -------------------------------------------------------------------------

-- TODO: they should have a clear path to "rigorous" AI-type projects

type alias App
  = { platform   : Platform
    , size       : Maybe Scale
    , quality    : Maybe Scale
    , importance : Maybe Scale
    , hosting    : Maybe Nod
    , category   : Maybe AppCategory
    , features   : Maybe AppFeatures
    , branding   : Maybe Branding
    }

type Platform = PlatformMobile Mobile
              | PlatformWeb

type AppCategory
-- TODO: each of these should have unique admin options
-- TODO: we may want to mov AppFeatures down into each alias record
  = Blog_      Blog
  | Store_     Store
  | Portfolio_ Portfolio
  | Game_      Game
  | Service_   Service
  | Advanced_  Service AdvancedApp 
  | Other_     Blog
               Store
               Portfolio
               Service
               Game
               AdvancedApp

type alias AppFeatures
  = { quick        : Maybe Bool -- 30-second version or 5-minute version?
    , design       : Maybe Scale
    , accounts     : Maybe AppAccounts
    , email        : Maybe AppEmail
    , search       : Maybe AppSearch
    , content      : Maybe AppContent
    , social       : Maybe AppSocial
    , commerce     : Maybe AppCommerce
    , external     : Maybe AppExternal
    , database     : Maybe AppDatabase
    , security     : Maybe Scale
    , integrations : Maybe Nod
    , contact      : Maybe Nod
    , management   : Maybe Nod
    , maintenance  : Maybe Nod
    , analytics    : Maybe Nod
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
-- TODO: do we need to provide content?
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

type AppDatabase
  = NewDatabase
  | ExistingDatabase
  | CloudDatabase
  | NoDatabase
  | WhatDatabase

type alias Mobile
  = { connect : Maybe Nod -- do people need to connect to your website?
    , icon    : Maybe Nod
    , push    : Maybe Nod
    }

type alias Blog
  = { cms      : Maybe Nod
    , comments : Maybe Nod
    }

type alias Store
  = { admin    : Maybe Nod
    , reviews  : Maybe Nod
    }

type alias Portfolio
  = { cms : Maybe Nod
    }

type alias Service
-- TODO: "buzzwords"
  = { imageProcessing : Maybe Nod
    , bigData         : Maybe Nod
    , cloudStorage    : Maybe Nod
    }

type alias AdvancedApp
-- TODO: give some of these a "fun" discount
  = { academic        : Maybe Nod
    , simulations     : Maybe Nod
    , textAnalysis    : Maybe Nod
    , ai              : Maybe Nod
    , iot             : Maybe Nod
    , parallelism     : Maybe Nod
    , metaprogramming : Maybe Nod
    , blockchain      : Maybe Nod
    }

type alias Game
  = { vr             : GameGraphics
    , inAppPurchases : Maybe Nod
    , multiplayer    : Maybe Nod
    }

type GameGraphics
  = Graphics2D
  | Graphics3D
  | GraphicsAR
  | GraphicsVR
  


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


-- AUTOMATION ------------------------------------------------------------------

-- TODO: in the beginning, we may want to just explain what it is, and then give the contact form

      
-- INIT ------------------------------------------------------------------------

init : Estimator
init
  = { project     = Nothing
    , impossible  = Nothing
    , urgency     = Nothing
    , nonProfit   = Nothing
    , paymentPlan = Nothing
    }


-- ESTIMATE --------------------------------------------------------------------

type alias Estimate
-- TODO: add uncertainty metric?
  = { impossible : EstimateVal
    , urgency    : EstimateVal
    , nonProfit  : EstimateVal
    , project    : EstimateProject
    }

type EstimateVal
-- TODO: add uncertainty metric?
  = Naught
  | Add      Int
  | Sub      Int
  | Discount Float
  | Multiply Float

type EstimateProject
  = EstimateBranding_ EstimateBranding
  | EstimateAutomation_
  | EstimateOther_

type alias EstimateBranding
  = { new        : EstimateVal
    , logo       : EstimateVal
    , social     : EstimateVal
    , guide      : EstimateVal
    , strategy   : EstimateVal
    , marketing  : EstimateVal
    , website    : EstimateVal
    , management : EstimateVal
    , content    : { copy   : EstimateVal
                   , images : EstimateVal
                   , video  : EstimateVal
                   }
    }

estimate : Estimator -> Estimate
estimate {project,impossible,urgency,nonProfit,paymentPlan}
  = { impossible
        = case impossible of
            Nothing    -> Naught
            Just True  -> Sub 250
            Just False -> Naught
    , urgency
        = case urgency of
            Nothing          -> Naught
            Just Yesterday   -> Multiply 100
            Just Today       -> Multiply 10
            Just ThisWeek    -> Multiply 4
            Just ThisMonth   -> Naught
            Just ThisQuarter -> Discount 0.01
            Just ThisYear    -> Discount 0.05
            Just ThisDecade  -> Discount 0.25
            Just Never       -> Multiply 0
    , nonProfit
        = case nonProfit of
            Nothing    -> Naught
            Just No    -> Naught
            Just Yes   -> Discount 0.05
            Just Dunno -> Naught
    , project
        = case project of

            -- ProjectApp app -> 

            -- ProjectImprovement improvement -> 

            Just (ProjectBranding {new,logo,social,content,guide,strategy,marketing,website,management}) -> 

              EstimateBranding_
              { new
                  = case new of
                      Nothing    -> Naught
                      Just True  -> Naught
                      Just False -> Naught
              , logo
                  = case logo of
                      Nothing    -> Naught
                      Just No    -> Naught
                      Just Yes   -> Naught
                      Just Dunno -> Naught
              , social
                  = case social of
                      Nothing    -> Naught
                      Just No    -> Naught
                      Just Yes   -> Naught
                      Just Dunno -> Naught
              , guide
                  = case guide of
                      Nothing    -> Naught
                      Just No    -> Naught
                      Just Yes   -> Naught
                      Just Dunno -> Naught
              , strategy
                  = case strategy of
                      Nothing    -> Naught
                      Just No    -> Naught
                      Just Yes   -> Naught
                      Just Dunno -> Naught
              , marketing
                  = case marketing of
                      Nothing    -> Naught
                      Just No    -> Naught
                      Just Yes   -> Naught
                      Just Dunno -> Naught
              , website
                  = case website of
                      Nothing     -> Naught
                      Just None   -> Naught
                      Just Low    -> Naught
                      Just Medium -> Naught
                      Just High   -> Naught
              , management
                  = case management of
                      Nothing    -> Naught
                      Just No    -> Naught
                      Just Yes   -> Naught
                      Just Dunno -> Naught
              , content
                  = case content of

                      Nothing ->

                        { copy   = Naught
                        , images = Naught
                        , video  = Naught
                        }
                        
                      Just content ->

                        { copy
                            = case content.copy of
                                Nothing     -> Naught
                                Just None   -> Naught
                                Just Low    -> Naught
                                Just Medium -> Naught
                                Just High   -> Naught
                        , images
                            = case content.images of
                                Nothing     -> Naught
                                Just None   -> Naught
                                Just Low    -> Naught
                                Just Medium -> Naught
                                Just High   -> Naught
                        , video
                            = case content.video of
                                Nothing     -> Naught
                                Just None   -> Naught
                                Just Low    -> Naught
                                Just Medium -> Naught
                                Just High   -> Naught
                        }
              }

            -- ProjectResearch -> 

            Just ProjectAutomation -> 

              EstimateAutomation_

            _ ->

              EstimateOther_

    }


-- VIEW ------------------------------------------------------------------------

view : Estimator -> Html msg
view est
  = text "TODO"

