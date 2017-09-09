{-# LANGUAGE OverloadedStrings #-}
-- | Automatically generated syntax definition for OpenCL.
-- DO NOT EDIT THIS FILE MANUALLY.
-- Instead, modify xml/opencl.xml and 'make bootstrap'.
module Skylighting.Syntax.Opencl (syntax) where

import Skylighting.Types
import Data.Binary

-- | Syntax definition for OpenCL.
syntax :: Syntax
syntax = decode "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKOpenCL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\nopencl.xml\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKOpencl\NUL\NUL\NUL\NUL\NUL\NUL\NUL\v\NUL\NUL\NUL\NUL\NUL\NUL\NUL\tAfterHash\NUL\NUL\NUL\NUL\NUL\NUL\NUL\tAfterHash\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKOpenCL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQ\ACK\NUL\NUL\NUL\NUL\NUL\NUL\NUL\FS#\\s*if(?:def|ndef)?(?=\\s+\\S)\NUL\DLE\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKOpenCL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\fPreprocessor\ACK\NUL\NUL\NUL\NUL\NUL\NUL\NUL\t#\\s*endif\NUL\DLE\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKOpenCL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\fPreprocessor\ACK\NUL\NUL\NUL\NUL\NUL\NUL\NUL\DC4#\\s*define.*((?=\\\\))\NUL\DLE\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKOpenCL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKDefine\ACK\NUL\NUL\NUL\NUL\NUL\NUL\NULL#\\s*(?:el(?:se|if)|include(?:_next)?|define|undef|line|error|warning|pragma)\NUL\DLE\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKOpenCL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\fPreprocessor\ACK\NUL\NUL\NUL\NUL\NUL\NUL\NUL\n#\\s+[0-9]+\NUL\DLE\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKOpenCL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\fPreprocessor\GS\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\vCommentar 1\NUL\NUL\NUL\NUL\NUL\NUL\NUL\vCommentar 1\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKOpenCL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\SO\f\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SI\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKAlerts\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\f\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\f\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\vCommentar 2\NUL\NUL\NUL\NUL\NUL\NUL\NUL\vCommentar 2\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKOpenCL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\SOH*/\f\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\SI\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKAlerts\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\f\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\f\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SYNCommentar/Preprocessor\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SYNCommentar/Preprocessor\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKOpenCL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\SOH*/\f\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\f\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKDefine\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKDefine\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKOpenCL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\SO\DLE\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\DLE\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKNormal\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKNormal\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKOpenCL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\DC4\DLE\RS\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACK\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SO#\\s*if\\s+0\\s*$\SOH\DLE\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKOpenCL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\tOutscoped\NUL#\RS\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKOpenCL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\tAfterHash\EOT\NUL\NUL\NUL\NUL\NUL\NUL\NUL\a//BEGIN\EM\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKOpenCL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\rRegion Marker\EOT\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQ//END\EM\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKOpenCL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\rRegion Marker\a\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\RS\t\n !\"%&'()*+,-./:;<=>?[\\]^{|}~\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL \NUL\NUL\NUL\NUL\NUL\NUL\NUL\n__constant\NUL\NUL\NUL\NUL\NUL\NUL\NUL\b__global\NUL\NUL\NUL\NUL\NUL\NUL\NUL\b__kernel\NUL\NUL\NUL\NUL\NUL\NUL\NUL\a__local\NUL\NUL\NUL\NUL\NUL\NUL\NUL\t__private\NUL\NUL\NUL\NUL\NUL\NUL\NUL\v__read_only\NUL\NUL\NUL\NUL\NUL\NUL\NUL\f__write_only\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQbreak\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOTcase\NUL\NUL\NUL\NUL\NUL\NUL\NUL\bconstant\NUL\NUL\NUL\NUL\NUL\NUL\NUL\bcontinue\NUL\NUL\NUL\NUL\NUL\NUL\NUL\adefault\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STXdo\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOTelse\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOTenum\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETXfor\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKglobal\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOTgoto\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STXif\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKinline\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKkernel\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQlocal\NUL\NUL\NUL\NUL\NUL\NUL\NUL\aprivate\NUL\NUL\NUL\NUL\NUL\NUL\NUL\tread_only\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKreturn\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKsizeof\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKstruct\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKswitch\NUL\NUL\NUL\NUL\NUL\NUL\NUL\atypedef\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQunion\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQwhile\NUL\NUL\NUL\NUL\NUL\NUL\NUL\nwrite_only\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\a\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\RS\t\n !\"%&'()*+,-./:;<=>?[\\]^{|}~\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NULO\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOTbool\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOTchar\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKchar16\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQchar2\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQchar3\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQchar4\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQchar8\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQconst\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKdouble\NUL\NUL\NUL\NUL\NUL\NUL\NUL\bdouble16\NUL\NUL\NUL\NUL\NUL\NUL\NUL\adouble2\NUL\NUL\NUL\NUL\NUL\NUL\NUL\adouble3\NUL\NUL\NUL\NUL\NUL\NUL\NUL\adouble4\NUL\NUL\NUL\NUL\NUL\NUL\NUL\adouble8\NUL\NUL\NUL\NUL\NUL\NUL\NUL\aevent_t\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQfloat\NUL\NUL\NUL\NUL\NUL\NUL\NUL\afloat16\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKfloat2\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKfloat3\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKfloat4\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKfloat8\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOThalf\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKhalf16\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQhalf2\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQhalf3\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQhalf4\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQhalf8\NUL\NUL\NUL\NUL\NUL\NUL\NUL\timage1d_t\NUL\NUL\NUL\NUL\NUL\NUL\NUL\timage2d_t\NUL\NUL\NUL\NUL\NUL\NUL\NUL\timage3d_t\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETXint\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQint16\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOTint2\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOTint3\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOTint4\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOTint8\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOTlong\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKlong16\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQlong2\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQlong3\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQlong4\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQlong8\NUL\NUL\NUL\NUL\NUL\NUL\NUL\brestrict\NUL\NUL\NUL\NUL\NUL\NUL\NUL\tsampler_t\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQshort\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ashort16\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKshort2\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKshort3\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKshort4\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKshort8\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKsigned\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKstatic\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQuchar\NUL\NUL\NUL\NUL\NUL\NUL\NUL\auchar16\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKuchar2\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKuchar3\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKuchar4\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKuchar8\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOTuint\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKuint16\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQuint2\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQuint3\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQuint4\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQuint8\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQulong\NUL\NUL\NUL\NUL\NUL\NUL\NUL\aulong16\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKulong2\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKulong3\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKulong4\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKulong8\NUL\NUL\NUL\NUL\NUL\NUL\NUL\bunsigned\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKushort\NUL\NUL\NUL\NUL\NUL\NUL\NUL\bushort16\NUL\NUL\NUL\NUL\NUL\NUL\NUL\aushort2\NUL\NUL\NUL\NUL\NUL\NUL\NUL\aushort3\NUL\NUL\NUL\NUL\NUL\NUL\NUL\aushort4\NUL\NUL\NUL\NUL\NUL\NUL\NUL\aushort8\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOTvoid\NUL\NUL\NUL\NUL\NUL\NUL\NUL\bvolatile\SOH\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\DC1\RS\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL{\RS\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL}\RS\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\t\EOT\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STXfF\EOT\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\n\ETX\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\v\ETX\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\b\STX\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\b\EOT\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETXULL\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOT\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETXLUL\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOT\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETXLLU\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOT\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STXUL\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOT\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STXLU\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOT\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STXLL\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOT\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOHU\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOT\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOHL\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\r\ACK\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\"\b\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKOpenCL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKString\SI\NUL\NUL\NUL\NUL\NUL\NUL\NUL\aDoxygen\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\RS\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH//\f\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKOpenCL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\vCommentar 1\SOH/*\f\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKOpenCL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\vCommentar 2\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SYN:!%&()+,-/.*<=>?[]|~^;\RS\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\RS\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\tOutscoped\NUL\NUL\NUL\NUL\NUL\NUL\NUL\tOutscoped\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKOpenCL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\n\DLE\f\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SI\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKAlerts\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\f\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\DC1\f\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\"\b\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKOpenCL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKString\SI\NUL\NUL\NUL\NUL\NUL\NUL\NUL\aDoxygen\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\f\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH//\f\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKOpenCL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\vCommentar 1\SOH/*\f\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKOpenCL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\vCommentar 2\ACK\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACK#\\s*if\SOH\f\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKOpenCL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\DLEOutscoped intern\ACK\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SI#\\s*el(?:se|if)\SOH\DLE\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\ACK\NUL\NUL\NUL\NUL\NUL\NUL\NUL\t#\\s*endif\SOH\DLE\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\f\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\DLEOutscoped intern\NUL\NUL\NUL\NUL\NUL\NUL\NUL\DLEOutscoped intern\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKOpenCL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\t\DLE\f\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SI\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKAlerts\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\f\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\DC1\f\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\"\b\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKOpenCL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKString\SI\NUL\NUL\NUL\NUL\NUL\NUL\NUL\aDoxygen\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\f\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH//\f\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKOpenCL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\vCommentar 1\SOH/*\f\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKOpenCL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\vCommentar 2\ACK\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACK#\\s*if\SOH\f\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKOpenCL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\DLEOutscoped intern\ACK\NUL\NUL\NUL\NUL\NUL\NUL\NUL\t#\\s*endif\SOH\f\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\f\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\fPreprocessor\NUL\NUL\NUL\NUL\NUL\NUL\NUL\fPreprocessor\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKOpenCL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACK\SO\DLE\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\"\"\DLE\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX<>\DLE\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SI\NUL\NUL\NUL\NUL\NUL\NUL\NUL\aDoxygen\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\DLE\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH/*\f\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKOpenCL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SYNCommentar/Preprocessor\SOH//\f\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKOpenCL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\vCommentar 1\DLE\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\rRegion Marker\NUL\NUL\NUL\NUL\NUL\NUL\NUL\rRegion Marker\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKOpenCL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EM\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKString\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKString\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKOpenCL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETX\SO\b\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\f\ACK\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\"\b\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\b\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH2\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOT*.cl\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ACKNormal"
