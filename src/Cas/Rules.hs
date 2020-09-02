module Cas.Rules where

import           PreludeCustom
import           Cas.Interface

substituteNode :: Rule -> T -> T
substituteNode (Rule x r) TX {..} | tX == x = r
substituteNode _          t                 = t

substitute :: Rule -> T -> T
substitute rule t = expand $ tmap (substituteNode rule) t

substituteInEqu :: Rule -> Equ -> Equ
substituteInEqu rule Equ {..} = Equ $ substitute rule equT

substituteInRuleBody :: Rule -> Rule -> Rule
substituteInRuleBody rule Rule {..} = Rule ruleX (substitute rule ruleT)

substituteInLinSys :: Rule -> LinSys -> LinSys
substituteInLinSys rule LinSys {..} = LinSys $ fmap (substituteInEqu rule) linSysEqus


