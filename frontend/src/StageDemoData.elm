module StageDemoData exposing (standardQuoteG, standardQuoteN, Plan)

type alias Plan =
    { price : Float
    , priceDiscount : Float
    , name : String
    , image : String
    , planType : String
    , naic : String
    , discountCategory : String
    }

-- Best Plan G option (Cigna)
standardQuoteG : Plan
standardQuoteG =
    { price = 123.94
    , priceDiscount = 116.50
    , name = "Cigna Health & Life Insurance Co"
    , image = "/images/Cigna.svg"
    , planType = "G"
    , naic = "67369"
    , discountCategory = "Multi-Insured / Roommate"
    }

-- Best Plan N option (Cigna)
standardQuoteN : Plan
standardQuoteN =
    { price = 90.11
    , priceDiscount = 84.70
    , name = "Cigna Health & Life Insurance Co"
    , image = "/images/Cigna.svg"
    , planType = "N"
    , naic = "67369"
    , discountCategory = "Multi-Insured / Roommate"
    }