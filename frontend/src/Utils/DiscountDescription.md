# Household Discount Description Module

This module fetches and parses household discount information from a CSV file.

## Usage

Here's how to use the updated module with HTTP fetching:

```elm
import Utils.DiscountDescription as HHD

-- In your model:
type alias Model =
    { hhdModel : HHD.HhdModel
      -- your other model fields
    }

-- Initialize with the model
init : Model
init =
    { hhdModel = HHD.init
      -- your other model initialization
    }

-- Add the HHD message to your messages
type Msg
    = HhdMsg HHD.HhdMsg
      -- your other messages

-- In your update function, handle the HHD messages
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HhdMsg hhdMsg ->
            let
                ( updatedHhdModel, hhdCmd ) =
                    HHD.update hhdMsg model.hhdModel
            in
            ( { model | hhdModel = updatedHhdModel }
            , Cmd.map HhdMsg hhdCmd
            )
        -- your other message handlers

-- In your init function or when you need to load the data
initCommands : Cmd Msg
initCommands =
    Cmd.batch
        [ Cmd.map HhdMsg HHD.fetchHhdData
          -- your other init commands
        ]

-- When you need to get a household discount description:
getDiscountInfo : Model -> Carrier -> String -> String -> Result String (Maybe String)
getDiscountInfo model carrier naic state =
    HHD.discountDescription model.hhdModel carrier naic state
```

## API

- `init` - Initialize the HHD model
- `fetchHhdData` - Command to fetch the CSV data from the server
- `update` - Update function to handle HTTP responses
- `discountDescription` - Get the household discount description for a specific carrier, NAIC, and state

## CSV Format

The CSV file should have the following format:

```
,ACE/Chubb,AETNA,AFLAC,Allstate,Anthem,CIGNA,Mutual of Omaha,Humana (MIPPA),Humana (Achieve),UHIC
AL,Description,Description,Description,...
AK,Description,Description,Description,...
...
```

The first column is the state code, and each subsequent column contains the discount description for a specific carrier. 