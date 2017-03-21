{-# language OverloadedStrings #-}
{-# language ExplicitForAll #-}
{-# language GADTs #-}

module Sally.SpockUtil where

import Web.Spock hiding (text)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

blaze html = do
    setHeader "Content-Type" "text/html; charset=utf-8"
    lazyBytes. renderHtml $ html



{-
 -
import Web.Spock.Config hiding (defaultSpockCfg)
import Web.Spock.Core (defaultSpockConfig, SpockConfig (..))
import Data.Pool
import Web.Spock.Internal.SessionVault
import Web.Spock.Internal.SessionManager
import Data.Vault.Lazy as V
import qualified Network.Wai as Wai

-- | Create a spock application using a given db storageLayer and an initial state.
-- Spock works with database libraries that already implement connection pooling and
-- with those that don't come with it out of the box. For more see the 'PoolOrConn' type.
-- Use @runSpock@ to run the app or @spockAsApp@ to create a @Wai.Application@
spock :: forall conn sess st. SpockCfg conn sess st -> SpockM conn sess st () -> IO Wai.Middleware
spock spockCfg spockAppl =
    do connectionPool <-
           case poolOrConn of
             PCNoDatabase ->
                 createPool (return ()) (const $ return ()) 5 60 5
             PCPool p ->
                 return p
             PCConn cb ->
                 let pc = cb_poolConfiguration cb
                 in createPool (cb_createConn cb) (cb_destroyConn cb)
                        (pc_stripes pc) (pc_keepOpenTime pc)
                        (pc_resPerStripe pc)
       internalState <-
           WebState
           <$> pure connectionPool
           <*> (createSessionManager sessionCfg $
                   SessionIf
                   { si_queryVault = queryVault
                   , si_modifyVault = modifyVault
                   , si_setRawMultiHeader = setRawMultiHeader
                   , si_vaultKey = V.newKey
                   }
               )
           <*> pure initialState
           <*> pure spockCfg
       let coreConfig =
               defaultSpockConfig
               { sc_maxRequestSize = spc_maxRequestSize spockCfg
               , sc_errorHandler = spc_errorHandler spockCfg
               }
       spockConfigT coreConfig (\m -> runResourceT $ runReaderT (runWebStateT m) internalState)  $
           do middleware (sm_middleware $ web_sessionMgr internalState)
              spockAppl
    where
        sessionCfg = spc_sessionCfg spockCfg
        poolOrConn = spc_database spockCfg
        initialState = spc_initialState spockCfg
-}
