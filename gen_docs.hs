import           Data.API.API.DSL
import           Data.API.Types
import           Data.API.Markdown


type URL     = String


file :: FilePath
file = "API.md"

mk_url :: TypeName -> URL
mk_url tn = _TypeName tn

main :: IO ()
main = writeFile file $ markdown "Type" mk_url apiAPI
