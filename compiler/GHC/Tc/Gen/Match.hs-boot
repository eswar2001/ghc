module GHC.Tc.Gen.Match where
import GHC.Hs           ( GRHSs, MatchGroup, LHsExpr, Mult )
import GHC.Tc.Types.Evidence  ( HsWrapper )
import GHC.Tc.Utils.TcType( ExpSigmaType, ExpRhoType )
import GHC.Tc.Types     ( TcM )
import GHC.Hs.Extension ( GhcRn, GhcTc )
import GHC.Parser.Annotation ( LocatedN )
import GHC.Types.Name (Name)

tcGRHSsPat    :: Mult
              -> GRHSs GhcRn (LHsExpr GhcRn)
              -> ExpRhoType
              -> TcM (GRHSs GhcTc (LHsExpr GhcTc))

tcFunBindMatches  :: LocatedN Name
                  -> Mult
                  -> MatchGroup GhcRn (LHsExpr GhcRn)
                  -> ExpSigmaType
                  -> TcM (HsWrapper, MatchGroup GhcTc (LHsExpr GhcTc))
