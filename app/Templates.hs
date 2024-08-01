module Templates where
import Development.Shake
import Data.Aeson (ToJSON)
import Data.Text (Text)
import qualified Text.Mustache as Mus
import qualified Text.Mustache.Compile as Mus
import qualified Development.Shake as Shake
import qualified Data.Aeson as A
import qualified Data.Text as T
import Development.Shake.FilePath ((</>))

applyTemplate :: ToJSON a => String -> a -> Action Text
applyTemplate templateName context = do
  tmpl <- readTemplate $ "templates" </> templateName
  case Mus.checkedSubstitute tmpl (A.toJSON context) of
    ([], text) -> return text
    (errs, _) -> fail $
      "Error while substituting template " <> templateName
        <> ": " <> unlines (map show errs)

applyTemplateAndWrite :: ToJSON a => String -> a -> FilePath -> Action ()
applyTemplateAndWrite templateName context outputPath =
  applyTemplate templateName context
    >>= Shake.writeFile' outputPath . T.unpack

readTemplate :: FilePath -> Action Mus.Template
readTemplate templatePath = do
  Shake.need [templatePath]
  eTemplate <- Shake.quietly
    . Shake.traced "Compile template"
    $ Mus.localAutomaticCompile templatePath
  case eTemplate of
    Right template -> do
      Shake.need . Mus.getPartials . Mus.ast $ template
      Shake.putInfo $ "Read " <> templatePath
      return template
    Left err -> fail $ show err
