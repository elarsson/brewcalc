module BeerXML.ParseMash (
getMashProfiles
,getMashProfile
)
where
import Data.Maybe
import Text.XML.Light
import Data.List.NonEmpty
import Ingredients.Common
import Ingredients.Mash
import BeerXML.Common
import Debug.Trace

getMashSteps :: Element -> [[MashStep]]
getMashSteps = getIngredients "MASH_STEPS" "MASH_STEP" getMashStep

getMashProfiles :: Element -> [[MashProfile]]
getMashProfiles e = getIngredients "MASHS" "MASH" getMashProfile e

getMashProfile :: Element -> Maybe MashProfile
getMashProfile e =
        let
            notes = getString $ findElement QName { qName = "NOTES", qURI = Nothing, qPrefix = Nothing } e
        in
        do
            name <- getString $ findElement QName { qName = "NAME", qURI = Nothing, qPrefix = Nothing } e
            version <- getValue $ findElement QName { qName = "VERSION", qURI = Nothing, qPrefix = Nothing } e
            grainTemp <- getValue $ findElement QName { qName = "GRAIN_TEMP", qURI = Nothing, qPrefix = Nothing } e
            mashSteps <- nonEmpty $ concat $ getMashSteps e
            return MashProfile
                {
                    Ingredients.Mash.profileName = name,
                    Ingredients.Mash.profileVersion = version,
                    Ingredients.Mash.grainTemp = grainTemp,
                    Ingredients.Mash.mashSteps = mashSteps,
                    Ingredients.Mash.notes = notes
                }

getMashStepType :: Maybe String -> Maybe Volume -> Maybe MashStepType
getMashStepType (Just "Infusion") (Just vol) = return (Infusion vol)
getMashStepType (Just "Decoction") _ = return Decoction
getMashStepType (Just "Temperature") _ = return Temperature
getMashStepType _ _ = Nothing

getMashStep :: Element -> Maybe MashStep
getMashStep e =
        let
            typeName = getString $ findElement QName { qName = "TYPE", qURI = Nothing, qPrefix = Nothing } e
            infuseAmount = getValue $ findElement QName { qName = "INFUSE_AMOUNT", qURI = Nothing, qPrefix = Nothing } e
            rampTime = getValue $ findElement QName { qName = "RAMP_TIME", qURI = Nothing, qPrefix = Nothing } e
            endTemp = getValue $ findElement QName { qName = "END_TEMP", qURI = Nothing, qPrefix = Nothing } e
        in
        do
            name <- getString $ findElement QName { qName = "NAME", qURI = Nothing, qPrefix = Nothing } e
            version <- getValue $ findElement QName { qName = "VERSION", qURI = Nothing, qPrefix = Nothing } e
            stepType <- getMashStepType typeName infuseAmount
            stepTemp <- getValue $ findElement QName { qName = "STEP_TEMP", qURI = Nothing, qPrefix = Nothing } e
            stepTime <- getValue $ findElement QName { qName = "STEP_TIME", qURI = Nothing, qPrefix = Nothing } e
            return MashStep
                {
                    Ingredients.Mash.stepName = name,
                    Ingredients.Mash.stepVersion = version,
                    Ingredients.Mash.stepType = stepType,
                    Ingredients.Mash.stepTemp = stepTemp,
                    Ingredients.Mash.stepTime = stepTime,
                    Ingredients.Mash.rampTime = rampTime,
                    Ingredients.Mash.endTemp = endTemp
                }
