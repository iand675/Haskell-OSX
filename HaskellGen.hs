module HaskellGen where
import           Data.Text (Text)
import qualified Data.Text as T
import           Filesystem
import           Filesystem.Path.CurrentOS
import           Prelude hiding (FilePath)

namespace :: Text -> Maybe Text -> Text -> Text -> Text
namespace prefix subname suffix framework = T.concat [prefix, maybe "" (T.cons '.') subname, ".", framework, ".", suffix]

frameworkDirs :: Framework -> [Text]
frameworkDirs (Framework name subs) = T.concat ["System/OSX/", name] : map (\s -> T.concat ["System/OSX/", name, "/", s]) subs

packages :: Framework -> [Text]
packages (Framework name subs) = T.append "osx-" name : map (T.append "osx-") subs

dependencies :: Framework -> [Text]
dependencies = tail . packages

baseFramework = namespace "System.OSX" Nothing "Primitive"
subFramework base = namespace "System.OSX" (Just base) "Primitive"

makeFrameworkDirs f = do
  let packageDirs = map (\d -> "framework" </> fromText d </> "src") $ packages f
      fwDirs = map fromText $ frameworkDirs f
      fullPaths = zipWith (</>) packageDirs fwDirs
  return fullPaths


data Framework = Framework Text [Text]

systemFrameworks = [ Framework "Accelerate" ["VecLib", "VImage"]
                   , Framework "AddressBook" []
                   , Framework "AGL" []
                   , Framework "AppKit" []
                   , Framework "AppleScriptKit" []
                   , Framework "AppleScriptObjC" []
                   , Framework "AppleShareClientCore" []
                   , Framework "ApplicationServices" ["ATS", "ColorSync", "CoreGraphics", "CoreText", "HIServices", "ImageIO", "LangAnalysis", "PrintCore", "QD", "SpeechSynthesis"]
                   , Framework "AudioToolbox" []
                   , Framework "AudioUnit" []
                   , Framework "Automator" ["MediaBrowser"]
                   , Framework "AVFoundation" []
                   , Framework "CalendarStore" []
                   , Framework "Carbon" ["CommonPanels", "Help", "HIToolbox", "ImageCapture", "Ink", "NavigationServices", "OpenScripting", "Print", "SecurityHI", "SpeechRecognition"]
                   , Framework "Cocoa" []
                   , Framework "Collaboration" []
                   , Framework "Cocoa" []
                   , Framework "CoreAudio" []
                   , Framework "CoreAudioKit" []
                   , Framework "CoreData" []
                   , Framework "CoreFoundation" []
                   , Framework "CoreLocation" []
                   , Framework "CoreMedia" []
                   , Framework "CoreMediaIO" []
                   , Framework "CoreMIDI" []
                   , Framework "CoreMIDIServer" []
                   , Framework "CoreServices" ["AE", "CarbonCore", "CFNetwork", "DictionaryServices", "LaunchServices", "Metadata", "OSServices", "SearchKit"]
                   , Framework "CoreVideo" []
                   , Framework "CoreWLAN" []
                   , Framework "Collaboration" []
                   , Framework "DirectoryService" []
                   , Framework "DiscRecording" []
                   , Framework "DiscRecordingUI" []
                   , Framework "DiskArbitration" []
                   , Framework "DrawSprocket" []
                   , Framework "DVComponentGlue" []
                   , Framework "DVDPlayback" []
                   , Framework "ExceptionHandling" []
                   , Framework "ForceFeedback" []
                   , Framework "Foundation" []
                   , Framework "FWAUserLib" []
                   , Framework "GLUT" []
                   , Framework "GSS" []
                   , Framework "ICADevices" []
                   , Framework "ImageCaptureCore" []
                   , Framework "IMServicePlugIn" []
                   , Framework "InputMethodKit" []
                   , Framework "InstallerPlugins" []
                   , Framework "InstantMessage" []
                   , Framework "IOBluetooth" []
                   , Framework "IOBluetoothUI" []
                   , Framework "IOSurface" []
                   , Framework "Kerberos" []
                   , Framework "Kernel" []
                   , Framework "LatentSemanticMapping" []
                   , Framework "Message" []
                   , Framework "NetFS" []
                   , Framework "OpenAL" []
                   , Framework "OpenCL" []
                   , Framework "OpenDirectory" []
                   , Framework "OpenGL" []
                   , Framework "OSAKit" []
                   , Framework "PCSC" []
                   , Framework "PreferencePanes" []
                   , Framework "PubSub" []
                   , Framework "QTKit" []
                   , Framework "Quartz" ["ImageKit", "PDFKit", "QuartzComposer", "QuartzFilters"]
                   , Framework "QuartzCore" []
                   , Framework "QuickLook" []
                   , Framework "QuickTime" []
                   , Framework "ScreenSaver" []
                   , Framework "ScriptingBridge" []
                   , Framework "Security" []
                   , Framework "SecurityFoundation" []
                   , Framework "SecurityInterface" []
                   , Framework "ServerNotification" []
                   , Framework "ServiceManagement" []
                   , Framework "SystemConfiguration" []
                   , Framework "TWAIN" []
                   , Framework "VideoDecodeAcceleration" []
                   , Framework "WebKit" ["WebCore"]
                   , Framework "XgridFoundation" []
                   , Framework "XPCService" []
                   ]

makeFrameworkList = concatMap (\(Framework base subs) -> baseFramework base : map (subFramework base) subs) systemFrameworks
