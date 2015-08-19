
;;;; Parser package.
(in-package :jsimple-parser)

;;; Lexer constants used as arguments in lexer construction.
;;; File js-syntax contains a syntax forest.
(defparameter +decimal-digit+ "[:digit:]")
(defparameter +decimal-digits+ "[:digit:]+")
(defparameter +non-zero-digit+ "[1-9]")
(defparameter +octal-digit+ "[0-7]")
(defparameter +hex-digit+ "[:xdigit:]")
(defparameter +unicode-identifier-start+ "[\xaa\xb5\xba\xc0-\xd6\xd8-\xf6\xf8-\u02c1\u02c6-\u02d1\u02e0-\u02e4\u02ec\u02ee\u0370-\u0374\u0376\u0377\u037a-\u037d\u0386\u0388-\u038a\u038c\u038e-\u03a1\u03a3-\u03f5\u03f7-\u0481\u048a-\u0527\u0531-\u0556\u0559\u0561-\u0587\u05d0-\u05ea\u05f0-\u05f2\u0620-\u064a\u066e\u066f\u0671-\u06d3\u06d5\u06e5\u06e6\u06ee\u06ef\u06fa-\u06fc\u06ff\u0710\u0712-\u072f\u074d-\u07a5\u07b1\u07ca-\u07ea\u07f4\u07f5\u07fa\u0800-\u0815\u081a\u0824\u0828\u0840-\u0858\u08a0\u08a2-\u08ac\u0904-\u0939\u093d\u0950\u0958-\u0961\u0971-\u0977\u0979-\u097f\u0985-\u098c\u098f\u0990\u0993-\u09a8\u09aa-\u09b0\u09b2\u09b6-\u09b9\u09bd\u09ce\u09dc\u09dd\u09df-\u09e1\u09f0\u09f1\u0a05-\u0a0a\u0a0f\u0a10\u0a13-\u0a28\u0a2a-\u0a30\u0a32\u0a33\u0a35\u0a36\u0a38\u0a39\u0a59-\u0a5c\u0a5e\u0a72-\u0a74\u0a85-\u0a8d\u0a8f-\u0a91\u0a93-\u0aa8\u0aaa-\u0ab0\u0ab2\u0ab3\u0ab5-\u0ab9\u0abd\u0ad0\u0ae0\u0ae1\u0b05-\u0b0c\u0b0f\u0b10\u0b13-\u0b28\u0b2a-\u0b30\u0b32\u0b33\u0b35-\u0b39\u0b3d\u0b5c\u0b5d\u0b5f-\u0b61\u0b71\u0b83\u0b85-\u0b8a\u0b8e-\u0b90\u0b92-\u0b95\u0b99\u0b9a\u0b9c\u0b9e\u0b9f\u0ba3\u0ba4\u0ba8-\u0baa\u0bae-\u0bb9\u0bd0\u0c05-\u0c0c\u0c0e-\u0c10\u0c12-\u0c28\u0c2a-\u0c33\u0c35-\u0c39\u0c3d\u0c58\u0c59\u0c60\u0c61\u0c85-\u0c8c\u0c8e-\u0c90\u0c92-\u0ca8\u0caa-\u0cb3\u0cb5-\u0cb9\u0cbd\u0cde\u0ce0\u0ce1\u0cf1\u0cf2\u0d05-\u0d0c\u0d0e-\u0d10\u0d12-\u0d3a\u0d3d\u0d4e\u0d60\u0d61\u0d7a-\u0d7f\u0d85-\u0d96\u0d9a-\u0db1\u0db3-\u0dbb\u0dbd\u0dc0-\u0dc6\u0e01-\u0e30\u0e32\u0e33\u0e40-\u0e46\u0e81\u0e82\u0e84\u0e87\u0e88\u0e8a\u0e8d\u0e94-\u0e97\u0e99-\u0e9f\u0ea1-\u0ea3\u0ea5\u0ea7\u0eaa\u0eab\u0ead-\u0eb0\u0eb2\u0eb3\u0ebd\u0ec0-\u0ec4\u0ec6\u0edc-\u0edf\u0f00\u0f40-\u0f47\u0f49-\u0f6c\u0f88-\u0f8c\u1000-\u102a\u103f\u1050-\u1055\u105a-\u105d\u1061\u1065\u1066\u106e-\u1070\u1075-\u1081\u108e\u10a0-\u10c5\u10c7\u10cd\u10d0-\u10fa\u10fc-\u1248\u124a-\u124d\u1250-\u1256\u1258\u125a-\u125d\u1260-\u1288\u128a-\u128d\u1290-\u12b0\u12b2-\u12b5\u12b8-\u12be\u12c0\u12c2-\u12c5\u12c8-\u12d6\u12d8-\u1310\u1312-\u1315\u1318-\u135a\u1380-\u138f\u13a0-\u13f4\u1401-\u166c\u166f-\u167f\u1681-\u169a\u16a0-\u16ea\u16ee-\u16f0\u1700-\u170c\u170e-\u1711\u1720-\u1731\u1740-\u1751\u1760-\u176c\u176e-\u1770\u1780-\u17b3\u17d7\u17dc\u1820-\u1877\u1880-\u18a8\u18aa\u18b0-\u18f5\u1900-\u191c\u1950-\u196d\u1970-\u1974\u1980-\u19ab\u19c1-\u19c7\u1a00-\u1a16\u1a20-\u1a54\u1aa7\u1b05-\u1b33\u1b45-\u1b4b\u1b83-\u1ba0\u1bae\u1baf\u1bba-\u1be5\u1c00-\u1c23\u1c4d-\u1c4f\u1c5a-\u1c7d\u1ce9-\u1cec\u1cee-\u1cf1\u1cf5\u1cf6\u1d00-\u1dbf\u1e00-\u1f15\u1f18-\u1f1d\u1f20-\u1f45\u1f48-\u1f4d\u1f50-\u1f57\u1f59\u1f5b\u1f5d\u1f5f-\u1f7d\u1f80-\u1fb4\u1fb6-\u1fbc\u1fbe\u1fc2-\u1fc4\u1fc6-\u1fcc\u1fd0-\u1fd3\u1fd6-\u1fdb\u1fe0-\u1fec\u1ff2-\u1ff4\u1ff6-\u1ffc\u2071\u207f\u2090-\u209c\u2102\u2107\u210a-\u2113\u2115\u2119-\u211d\u2124\u2126\u2128\u212a-\u212d\u212f-\u2139\u213c-\u213f\u2145-\u2149\u214e\u2160-\u2188\u2c00-\u2c2e\u2c30-\u2c5e\u2c60-\u2ce4\u2ceb-\u2cee\u2cf2\u2cf3\u2d00-\u2d25\u2d27\u2d2d\u2d30-\u2d67\u2d6f\u2d80-\u2d96\u2da0-\u2da6\u2da8-\u2dae\u2db0-\u2db6\u2db8-\u2dbe\u2dc0-\u2dc6\u2dc8-\u2dce\u2dd0-\u2dd6\u2dd8-\u2dde\u2e2f\u3005-\u3007\u3021-\u3029\u3031-\u3035\u3038-\u303c\u3041-\u3096\u309d-\u309f\u30a1-\u30fa\u30fc-\u30ff\u3105-\u312d\u3131-\u318e\u31a0-\u31ba\u31f0-\u31ff\u3400-\u4db5\u4e00-\u9fcc\ua000-\ua48c\ua4d0-\ua4fd\ua500-\ua60c\ua610-\ua61f\ua62a\ua62b\ua640-\ua66e\ua67f-\ua697\ua6a0-\ua6ef\ua717-\ua71f\ua722-\ua788\ua78b-\ua78e\ua790-\ua793\ua7a0-\ua7aa\ua7f8-\ua801\ua803-\ua805\ua807-\ua80a\ua80c-\ua822\ua840-\ua873\ua882-\ua8b3\ua8f2-\ua8f7\ua8fb\ua90a-\ua925\ua930-\ua946\ua960-\ua97c\ua984-\ua9b2\ua9cf\uaa00-\uaa28\uaa40-\uaa42\uaa44-\uaa4b\uaa60-\uaa76\uaa7a\uaa80-\uaaaf\uaab1\uaab5\uaab6\uaab9-\uaabd\uaac0\uaac2\uaadb-\uaadd\uaae0-\uaaea\uaaf2-\uaaf4\uab01-\uab06\uab09-\uab0e\uab11-\uab16\uab20-\uab26\uab28-\uab2e\uabc0-\uabe2\uac00-\ud7a3\ud7b0-\ud7c6\ud7cb-\ud7fb\uf900-\ufa6d\ufa70-\ufad9\ufb00-\ufb06\ufb13-\ufb17\ufb1d\ufb1f-\ufb28\ufb2a-\ufb36\ufb38-\ufb3c\ufb3e\ufb40\ufb41\ufb43\ufb44\ufb46-\ufbb1\ufbd3-\ufd3d\ufd50-\ufd8f\ufd92-\ufdc7\ufdf0-\ufdfb\ufe70-\ufe74\ufe76-\ufefc\uff21-\uff3a\uff41-\uff5a\uff66-\uffbe\uffc2-\uffc7\uffca-\uffcf\uffd2-\uffd7\uffda-\uffdc]")
(defparameter +unicode-identifier-part+ "[\xaa\xb5\xba\xc0-\xd6\xd8-\xf6\xf8-\u02c1\u02c6-\u02d1\u02e0-\u02e4\u02ec\u02ee\u0370-\u0374\u0376\u0377\u037a-\u037d\u0386\u0388-\u038a\u038c\u038e-\u03a1\u03a3-\u03f5\u03f7-\u0481\u048a-\u0527\u0531-\u0556\u0559\u0561-\u0587\u05d0-\u05ea\u05f0-\u05f2\u0620-\u064a\u066e\u066f\u0671-\u06d3\u06d5\u06e5\u06e6\u06ee\u06ef\u06fa-\u06fc\u06ff\u0710\u0712-\u072f\u074d-\u07a5\u07b1\u07ca-\u07ea\u07f4\u07f5\u07fa\u0800-\u0815\u081a\u0824\u0828\u0840-\u0858\u08a0\u08a2-\u08ac\u0904-\u0939\u093d\u0950\u0958-\u0961\u0971-\u0977\u0979-\u097f\u0985-\u098c\u098f\u0990\u0993-\u09a8\u09aa-\u09b0\u09b2\u09b6-\u09b9\u09bd\u09ce\u09dc\u09dd\u09df-\u09e1\u09f0\u09f1\u0a05-\u0a0a\u0a0f\u0a10\u0a13-\u0a28\u0a2a-\u0a30\u0a32\u0a33\u0a35\u0a36\u0a38\u0a39\u0a59-\u0a5c\u0a5e\u0a72-\u0a74\u0a85-\u0a8d\u0a8f-\u0a91\u0a93-\u0aa8\u0aaa-\u0ab0\u0ab2\u0ab3\u0ab5-\u0ab9\u0abd\u0ad0\u0ae0\u0ae1\u0b05-\u0b0c\u0b0f\u0b10\u0b13-\u0b28\u0b2a-\u0b30\u0b32\u0b33\u0b35-\u0b39\u0b3d\u0b5c\u0b5d\u0b5f-\u0b61\u0b71\u0b83\u0b85-\u0b8a\u0b8e-\u0b90\u0b92-\u0b95\u0b99\u0b9a\u0b9c\u0b9e\u0b9f\u0ba3\u0ba4\u0ba8-\u0baa\u0bae-\u0bb9\u0bd0\u0c05-\u0c0c\u0c0e-\u0c10\u0c12-\u0c28\u0c2a-\u0c33\u0c35-\u0c39\u0c3d\u0c58\u0c59\u0c60\u0c61\u0c85-\u0c8c\u0c8e-\u0c90\u0c92-\u0ca8\u0caa-\u0cb3\u0cb5-\u0cb9\u0cbd\u0cde\u0ce0\u0ce1\u0cf1\u0cf2\u0d05-\u0d0c\u0d0e-\u0d10\u0d12-\u0d3a\u0d3d\u0d4e\u0d60\u0d61\u0d7a-\u0d7f\u0d85-\u0d96\u0d9a-\u0db1\u0db3-\u0dbb\u0dbd\u0dc0-\u0dc6\u0e01-\u0e30\u0e32\u0e33\u0e40-\u0e46\u0e81\u0e82\u0e84\u0e87\u0e88\u0e8a\u0e8d\u0e94-\u0e97\u0e99-\u0e9f\u0ea1-\u0ea3\u0ea5\u0ea7\u0eaa\u0eab\u0ead-\u0eb0\u0eb2\u0eb3\u0ebd\u0ec0-\u0ec4\u0ec6\u0edc-\u0edf\u0f00\u0f40-\u0f47\u0f49-\u0f6c\u0f88-\u0f8c\u1000-\u102a\u103f\u1050-\u1055\u105a-\u105d\u1061\u1065\u1066\u106e-\u1070\u1075-\u1081\u108e\u10a0-\u10c5\u10c7\u10cd\u10d0-\u10fa\u10fc-\u1248\u124a-\u124d\u1250-\u1256\u1258\u125a-\u125d\u1260-\u1288\u128a-\u128d\u1290-\u12b0\u12b2-\u12b5\u12b8-\u12be\u12c0\u12c2-\u12c5\u12c8-\u12d6\u12d8-\u1310\u1312-\u1315\u1318-\u135a\u1380-\u138f\u13a0-\u13f4\u1401-\u166c\u166f-\u167f\u1681-\u169a\u16a0-\u16ea\u16ee-\u16f0\u1700-\u170c\u170e-\u1711\u1720-\u1731\u1740-\u1751\u1760-\u176c\u176e-\u1770\u1780-\u17b3\u17d7\u17dc\u1820-\u1877\u1880-\u18a8\u18aa\u18b0-\u18f5\u1900-\u191c\u1950-\u196d\u1970-\u1974\u1980-\u19ab\u19c1-\u19c7\u1a00-\u1a16\u1a20-\u1a54\u1aa7\u1b05-\u1b33\u1b45-\u1b4b\u1b83-\u1ba0\u1bae\u1baf\u1bba-\u1be5\u1c00-\u1c23\u1c4d-\u1c4f\u1c5a-\u1c7d\u1ce9-\u1cec\u1cee-\u1cf1\u1cf5\u1cf6\u1d00-\u1dbf\u1e00-\u1f15\u1f18-\u1f1d\u1f20-\u1f45\u1f48-\u1f4d\u1f50-\u1f57\u1f59\u1f5b\u1f5d\u1f5f-\u1f7d\u1f80-\u1fb4\u1fb6-\u1fbc\u1fbe\u1fc2-\u1fc4\u1fc6-\u1fcc\u1fd0-\u1fd3\u1fd6-\u1fdb\u1fe0-\u1fec\u1ff2-\u1ff4\u1ff6-\u1ffc\u2071\u207f\u2090-\u209c\u2102\u2107\u210a-\u2113\u2115\u2119-\u211d\u2124\u2126\u2128\u212a-\u212d\u212f-\u2139\u213c-\u213f\u2145-\u2149\u214e\u2160-\u2188\u2c00-\u2c2e\u2c30-\u2c5e\u2c60-\u2ce4\u2ceb-\u2cee\u2cf2\u2cf3\u2d00-\u2d25\u2d27\u2d2d\u2d30-\u2d67\u2d6f\u2d80-\u2d96\u2da0-\u2da6\u2da8-\u2dae\u2db0-\u2db6\u2db8-\u2dbe\u2dc0-\u2dc6\u2dc8-\u2dce\u2dd0-\u2dd6\u2dd8-\u2dde\u2e2f\u3005-\u3007\u3021-\u3029\u3031-\u3035\u3038-\u303c\u3041-\u3096\u309d-\u309f\u30a1-\u30fa\u30fc-\u30ff\u3105-\u312d\u3131-\u318e\u31a0-\u31ba\u31f0-\u31ff\u3400-\u4db5\u4e00-\u9fcc\ua000-\ua48c\ua4d0-\ua4fd\ua500-\ua60c\ua610-\ua61f\ua62a\ua62b\ua640-\ua66e\ua67f-\ua697\ua6a0-\ua6ef\ua717-\ua71f\ua722-\ua788\ua78b-\ua78e\ua790-\ua793\ua7a0-\ua7aa\ua7f8-\ua801\ua803-\ua805\ua807-\ua80a\ua80c-\ua822\ua840-\ua873\ua882-\ua8b3\ua8f2-\ua8f7\ua8fb\ua90a-\ua925\ua930-\ua946\ua960-\ua97c\ua984-\ua9b2\ua9cf\uaa00-\uaa28\uaa40-\uaa42\uaa44-\uaa4b\uaa60-\uaa76\uaa7a\uaa80-\uaaaf\uaab1\uaab5\uaab6\uaab9-\uaabd\uaac0\uaac2\uaadb-\uaadd\uaae0-\uaaea\uaaf2-\uaaf4\uab01-\uab06\uab09-\uab0e\uab11-\uab16\uab20-\uab26\uab28-\uab2e\uabc0-\uabe2\uac00-\ud7a3\ud7b0-\ud7c6\ud7cb-\ud7fb\uf900-\ufa6d\ufa70-\ufad9\ufb00-\ufb06\ufb13-\ufb17\ufb1d\ufb1f-\ufb28\ufb2a-\ufb36\ufb38-\ufb3c\ufb3e\ufb40\ufb41\ufb43\ufb44\ufb46-\ufbb1\ufbd3-\ufd3d\ufd50-\ufd8f\ufd92-\ufdc7\ufdf0-\ufdfb\ufe70-\ufe74\ufe76-\ufefc\uff21-\uff3a\uff41-\uff5a\uff66-\uffbe\uffc2-\uffc7\uffca-\uffcf\uffd2-\uffd7\uffda-\uffdc0-9\u0300-\u036f\u0483-\u0487\u0591-\u05bd\u05bf\u05c1\u05c2\u05c4\u05c5\u05c7\u0610-\u061a\u064b-\u0669\u0670\u06d6-\u06dc\u06df-\u06e4\u06e7\u06e8\u06ea-\u06ed\u06f0-\u06f9\u0711\u0730-\u074a\u07a6-\u07b0\u07c0-\u07c9\u07eb-\u07f3\u0816-\u0819\u081b-\u0823\u0825-\u0827\u0829-\u082d\u0859-\u085b\u08e4-\u08fe\u0900-\u0903\u093a-\u093c\u093e-\u094f\u0951-\u0957\u0962\u0963\u0966-\u096f\u0981-\u0983\u09bc\u09be-\u09c4\u09c7\u09c8\u09cb-\u09cd\u09d7\u09e2\u09e3\u09e6-\u09ef\u0a01-\u0a03\u0a3c\u0a3e-\u0a42\u0a47\u0a48\u0a4b-\u0a4d\u0a51\u0a66-\u0a71\u0a75\u0a81-\u0a83\u0abc\u0abe-\u0ac5\u0ac7-\u0ac9\u0acb-\u0acd\u0ae2\u0ae3\u0ae6-\u0aef\u0b01-\u0b03\u0b3c\u0b3e-\u0b44\u0b47\u0b48\u0b4b-\u0b4d\u0b56\u0b57\u0b62\u0b63\u0b66-\u0b6f\u0b82\u0bbe-\u0bc2\u0bc6-\u0bc8\u0bca-\u0bcd\u0bd7\u0be6-\u0bef\u0c01-\u0c03\u0c3e-\u0c44\u0c46-\u0c48\u0c4a-\u0c4d\u0c55\u0c56\u0c62\u0c63\u0c66-\u0c6f\u0c82\u0c83\u0cbc\u0cbe-\u0cc4\u0cc6-\u0cc8\u0cca-\u0ccd\u0cd5\u0cd6\u0ce2\u0ce3\u0ce6-\u0cef\u0d02\u0d03\u0d3e-\u0d44\u0d46-\u0d48\u0d4a-\u0d4d\u0d57\u0d62\u0d63\u0d66-\u0d6f\u0d82\u0d83\u0dca\u0dcf-\u0dd4\u0dd6\u0dd8-\u0ddf\u0df2\u0df3\u0e31\u0e34-\u0e3a\u0e47-\u0e4e\u0e50-\u0e59\u0eb1\u0eb4-\u0eb9\u0ebb\u0ebc\u0ec8-\u0ecd\u0ed0-\u0ed9\u0f18\u0f19\u0f20-\u0f29\u0f35\u0f37\u0f39\u0f3e\u0f3f\u0f71-\u0f84\u0f86\u0f87\u0f8d-\u0f97\u0f99-\u0fbc\u0fc6\u102b-\u103e\u1040-\u1049\u1056-\u1059\u105e-\u1060\u1062-\u1064\u1067-\u106d\u1071-\u1074\u1082-\u108d\u108f-\u109d\u135d-\u135f\u1712-\u1714\u1732-\u1734\u1752\u1753\u1772\u1773\u17b4-\u17d3\u17dd\u17e0-\u17e9\u180b-\u180d\u1810-\u1819\u18a9\u1920-\u192b\u1930-\u193b\u1946-\u194f\u19b0-\u19c0\u19c8\u19c9\u19d0-\u19d9\u1a17-\u1a1b\u1a55-\u1a5e\u1a60-\u1a7c\u1a7f-\u1a89\u1a90-\u1a99\u1b00-\u1b04\u1b34-\u1b44\u1b50-\u1b59\u1b6b-\u1b73\u1b80-\u1b82\u1ba1-\u1bad\u1bb0-\u1bb9\u1be6-\u1bf3\u1c24-\u1c37\u1c40-\u1c49\u1c50-\u1c59\u1cd0-\u1cd2\u1cd4-\u1ce8\u1ced\u1cf2-\u1cf4\u1dc0-\u1de6\u1dfc-\u1dff\u200c\u200d\u203f\u2040\u2054\u20d0-\u20dc\u20e1\u20e5-\u20f0\u2cef-\u2cf1\u2d7f\u2de0-\u2dff\u302a-\u302f\u3099\u309a\ua620-\ua629\ua66f\ua674-\ua67d\ua69f\ua6f0\ua6f1\ua802\ua806\ua80b\ua823-\ua827\ua880\ua881\ua8b4-\ua8c4\ua8d0-\ua8d9\ua8e0-\ua8f1\ua900-\ua909\ua926-\ua92d\ua947-\ua953\ua980-\ua983\ua9b3-\ua9c0\ua9d0-\ua9d9\uaa29-\uaa36\uaa43\uaa4c\uaa4d\uaa50-\uaa59\uaa7b\uaab0\uaab2-\uaab4\uaab7\uaab8\uaabe\uaabf\uaac1\uaaeb-\uaaef\uaaf5\uaaf6\uabe3-\uabea\uabec\uabed\uabf0-\uabf9\ufb1e\ufe00-\ufe0f\ufe20-\ufe26\ufe33\ufe34\ufe4d-\ufe4f\uff10-\uff19\uff3f]")
;;; TODO: Use a inline function to remove the #\[ and #\] around, then cat.
(defparameter +identifier-start+
  +unicode-identifier-start+|[$_a-zA-Z]|("\\"[u]{+hex-digit+}{4}))
(defparameter +identifier-part+
  +identifier-start+|+unicode-identifier-part+|[:digit:])
(defparameter +identifier+
  (concatenate 'string +identifier-start+ +identifier-part+ "*"))
(defparameter +exponent-indicator+ "[eE]")
(defparameter +signed-integer+ "[+-]?[0-9]+")
(defparameter +decimal-integer-literal+
  (concatenate 'string "[0]\|" +non-zero-digit+ +decimal-digits "*"))
(defparameter +exponent-part+
  (concatenete 'string +exponent-indicator+ +signed-integer+))
(defparameter +octal-integer-literal+
  (concatenate 'string "[0]" +octal-digit+ "*"))
(defparameter +hex-integer-literal+
  (concatenate 'string "[0][xX]" +hex-digit+ "*"))
(defparameter +decimal-literal+
  (concatenate 'string
	       +decimal-integer-literal+
	       "\." +decimal-digits+ "*"
	       ;;
	       ))


;;; Since Lisp is written with prefix operators, we have to convert it into
;;; javascript-land style.
(defun infix-to-prefix (a op b)
  (list op a b))

;;; Similarly, a++ will be translated into (INCF A) with no optimization.
(defun suffix-to-prefix (a op)
  (list op a))

;;; Select the second argument.
;;; TODO: make all those functions generic.
(defun snd-2 (f s)
  (declare (ignore f))
  s)

(defun snd-3 (f s t)
  (declare (ignore f t))
  s)

;;; Push element, become the tail of current list.
(defun push-2 (lst obj)
  (let ((out (reverse lst)))
    (reverse (push obj out))))

(defun push-3 (lst obj1 obj2)
  )

;;; Lexer generated by CL-LEX.
(deflexer jsimple-lexer ("[0-9]+([.][0-9]+([Ee][0-9]+)?)"
			 (return (values 'float (num %0))))
  ("[0-9]+" (return (values 'int (int %0))))
  ("[:alpha:][:alnum:]*" (return (values 'id %0)))
  ("[:space:]+")
  ())

(define-parser *jsimple-parser*
  ;; FIXME: where to start?
  (:start-symbol program)
  (:terminals
   (number
    id
    + ++ - --
    * / %
    += -= *= /= %= = == === != !==
    > < >= <=
    |(| |)|
    break do in typeof case else instanceof var catch export new void class
    extends return while const finally super with continue for switch yield
    debugger function this default if throw delete import try))
  (:precedence ((:left * / %) (:left + -)))

  (statement
   block-statement
   variable-statement
   empty-statement
   expression-statement
   if-statement
   iteration-statement
   continue-statement
   break-statement
   return-statement
   with-statement
   labelled-statement
   switch-statement
   throw-statement
   try-statement
   debugger-statement)

  (block-statement
   ({ statement-list } #'build-block-statement))

  (statement-list
   (statement-list statement #'push-2)
   nil)

  (variable-statement
   (var variable-declaration-list #'build-variable-declaration))

  (variable-declaration-list
   (variable-declaration #'list)
   (variable-declaration-list , variable-declaration #'push-3))

  (variable-declaration-list-no-in
   (variable-declaration-no-in #'list)
   (variable-declaration-list-no-in , variable-declaration-no-in #'push-3))

  (variable-declaration
   (id #'build-variable-declaration)
   (id initializer #'build-variable-declaration))

  (variable-declaration-no-in
   (id #'build-variable-declaration)
   (id initializer-no-in #'build-variable-declaration))

  (initializer
   (= assignment-expression #'snd-2))

  (initializer-no-in
   (= assignment-expression-no-in #'snd-2))

  (empty-statement
   (#\; #'build-empty-statement))

  (expression-statement
   (expression-no-bf #\; #'build-expression-statement)
   (expression-no-bf error #'build-expression-statement))

  (if-statement
   (if { expression } statement #'build-if-statement)
   (if { expression } statement else statement #'build-if-statement))

  (iteration-statement
   (do statement while ( expression ) #\; #'build-do-while-statement)
   (do statement while ( expression ) error #'build-do-while-statement)
   (while ( expression ) statement #'build-while-statement)
   (for ( expression-no-in #\; expression #\; expression ) statement #'build-for-statement)
   (for ( expression-no-in #\; expression #\; ) statement #'build-for-statement)
   (for ( expression-no-in #\; #\; expression ) statement #'build-for-statement)
   (for ( expression-no-in #\; #\; ) statement #'build-for-statement)
   (for ( #\; expression #\; expression ) statement #'build-for-statement)
   (for ( #\; expression #\; ) statement #'build-for-statement)
   (for ( #\; #\; expression ) statement #'build-for-statement)
   (for ( #\; #\; ) statement #'build-for-statement)
   (for ( var variable-declaration-list-no-in #\; expression #\; expression ) statement #'build-for-statement)
   (for ( var variable-declaration-list-no-in #\; expression #\; ) statement #'build-for-statement)
   (for ( var variable-declaration-list-no-in #\; expression ) statement #'build-for-statement)
   (for ( var variable-declaration-list-no-in #\; ) statement #'build-for-statement)
   (for ( left-hand-side-expression in expression ) statement #'build-for-in-statement)
   (for ( var variable-declaration-no-in in expression ) statement #'build-for-in-statement))

  (continue-statement
   (continue #\; #'build-continue-statement)
   (continue error #'build-continue-statement)
   (continue id #\; #'build-continue-statement)
   (continue id error #'build-continue-statement))

  (break-statement
   (break #\; #'build-break-statement)
   (break error #'build-break-statement)
   (break id #\; #'build-break-statement)
   (break id error #'build-break-statement))

  (return-statement
   (return #\; #'build-return-statement)
   (return error #'build-return-statement)
   (return expression #\; #'build-return-statement)
   (return expression error #'build-return-statement))

  (with-statement
   (with ( expression ) statement #'build-with-statement))

  (switch-statement
   (switch ( expression ) case-block #'build-switch-statement))

  (case-block
   ({ case-clauses } #'snd-3)
   ({ case-clauses default-clause case-clauses } #'push-3))

  (case-clauses
   (case-clauses case-clause #'push-2)
   nil)

  (case-clause
   (case expression : statement-list #'build-switch-case))

  (default-clause
   (default : statement-list #'build-switch-case))

  (labelled-statement
   (id : statement #'build-labelled-statement))

  (throw-statement
   (throw expression #\; #'build-throw-statement)
   (throw expression error #'build-throw-statement))

  (try-statement
   (try block-statement catch #'build-try-statement)
   (try block-statement finally #'build-try-statement)
   (try block-statement catch finally #'build-try-statement))

  (catch
   (catch ( id ) block-statment #'build-catch-clause))

  (finally
   (finally block-statement #'snd-2))

  (debugger-statement
   (debugger #\; #'build-debugger-statement)
   (debugger error #'build-debugger-statement))

  (function-declaration
   (function id ( ) { function-body } #'build-function-declaration)
   (function id ( formal-parameter-list ) { function-body } #'build-function-declaration))

  (function-expression
   (function id ( ) { function-body } #'build-function-expression)
   (function id ( formal-parameter-list ) { function-body } #'build-function-expression)
   (function ( ) { function-body } #'build-function-expression)
   (function ( formal-parameter-list ) { function-body } #'build-function-expression))

  ;; TODO: Is hashtable really required?
  (formal-parameter-list
   (id #'build-identifier)
   (formal-parameter-list , id #'push-2))

  (function-body
   source-elements)

  (program
   (source-elements ;;EOF
    #'build-program))
  
  (source-elements
   (source-elements source-element #'push-2)
   nil)

  (source-element
   statement
   function-declaration)

  (primary-expression
   primary-expression-no-brace
   object-literal)

  (primary-expression-no-brace
   (this #'build-this-expression)
   (id #'build-identifier)
   (literal)
   (array-literal)
   ({ expression } #'snd-3))

  (array-literal
   ([ ] #'build-array-expression)
   ([ elision ] #'build-array-expression)
   ([ element-list ] #'build-array-expression)
   ([ element-list , ] #'build-array-expression)
   ([ element-list , elision ] #'build-array-expression))

  (element-list
   (assignment-expression #'list)
   (elision assignment-expression #'push-2)
   (element-list , assignment-expression #'push-2)
   (element-list , elision assignment-expression #'push-3))

  (elision
   (, #'null-list)
   (elision , #'push-2))

  (object-literal
   ({ } #'build-object-expression)
   ({ property-name-and-value-list } #'build-object-expression)
   ({ property-name-and-value-list , } #'build-object-expression))

  (property-name-and-value-list
   (property-assignment #'list)
   (property-name-and-value-list , property-assignment #'push-2))

  (property-assignment
   (property-name : assignment-expression #'build-property)
   ;; How to tell difference between getter and others?
   (id property-name ( ) { function-body } ;;
       )
   ;; And setter...
   (id property-name ( property-set-parameter-list ) { function-body } ;;
       ))

  (property-name
   identifier-name
   string-literal
   numeric-literal)

  (property-set-parameter-list
   (id #'build-identifier))

  (member-expression
   primary-expression
   function-expression
   (member-expression [ expression ] #'build-member-expression)
   (member-expression . id #'build-member-expression)
   (new member-expression arguments #'build-member-expression))

  (member-expression-no-bf
   primary-expression-no-brace
   (member-expression-no-bf [ expression ] #'build-member-expression)
   (member-expression-no-bf . id #'build-member-expression)
   (new member-expression arguments #'build-member-expression))

  (new-expression
   member-expression
   (new new-expression #'build-new-expression))

  (new-expression-no-bf
   member-expression
   (new new-expression #'build-new-expression))

  (call-expression
   (member-expression arguments #'build-call-expression)
   (call-expression arguments #'build-call-expression)
   (call-expression [ expression ] #'build-member-expression)
   (call-expression . id #'build-member-expression))

  (call-expression-no-bf
   (member-expression-no-bf arguments #'build-call-expression)
   (call-expression-no-bf arguments #'build-call-expression)
   (call-expression-no-bf [ expression ] #'build-member-expression)
   (call-expression-no-bf . id #'build-member-expression))

  (id
   (id #'build-identifier)
   (reserved-word #'build-identifier))

  (arguments
   (( ) nil)
   (( argument-list ) #'snd-3))

  (argument-list
   (assignment-expression #'list)
   (argument , assignment-expression #'push-2))

  (left-hand-side-expression
   new-expression
   call-expression)

  (left-hand-side-expression-no-bf
   new-expression-no-bf
   call-expression-no-bf)

  (postfix-expression
   left-hand-side-expression
   (left-hand-side-expression ++ #'build-update-expression)
   (left-hand-side-expression -- #'build-update-expression))

  (postfix-expression-no-bf
   left-hand-side-expression-no-bf
   (left-hand-side-expression-no-bf ++ #'build-update-expression)
   (left-hand-side-expression-no-bf -- #'build-update-expression))

  (unary-expression
   postfix-expression
   unary-expr)

  (unary-expression-no-bf
   postfix-expression-no-bf
   unary-expr)

  (unary-expr
   (delete unary-expression #'build-unary-expression)
   (void unary-expression #'build-unary-expression)
   (typeof unary-expression #'build-unary-expression)
   ;; FIXME: Update the position!
   (br++ unary-expression #'build-unary-expression)
   (br-- unary-expression #'build-unary-expression)
   (++ unary-expression #'build-unary-expression)
   (-- unary-expression #'build-unary-expression)
   (+ unary-expression #'build-unary-expression)
   (- unary-expression #'build-unary-expression)
   (~ unary-expression #'build-unary-expression)
   (! unary-expression #'build-unary-expression))

  (multiplicative-expression
   unary-expression
   (multiplicative-expression * unary-expression #'build-binary-expression)
   (multiplicative-expression / unary-expression #'build-binary-expression)
   (multiplicative-expression % unary-expression #'build-binary-expression))

  (multiplicative-expression-no-bf
   unary-expression-no-bf
   (multiplicative-expression-no-bf * unary-expression #'build-binary-expression)
   (multiplicative-expression-no-bf / unary-expression #'build-binary-expression)
   (multiplicative-expression-no-bf % unary-expression #'build-binary-expression))

  (additive-expression
   multiplicative-expression
   (additive-expression + multiplicative-expression #'build-binary-expression)
   (additive-expression - multiplicative-expression #'build-binary-expression))

  (additive-expression-no-bf
   multiplicative-expression-no-bf
   (additive-expression-no-bf + multiplicative-expression #'build-binary-expression)
   (additive-expression-no-bf - multiplicative-expression #'build-binary-expression))

  (shift-expression
   additive-expression
   (shift-expression << additive-expression #'build-binary-expression)
   (shift-expression >> additive-expression #'build-binary-expression)
   (shift-expression >>> additive-expression #'build-binary-expression))

  (shift-expression-no-bf
   additive-expression-no-bf
   (shift-expression-no-bf << additive-expression #'build-binary-expression)
   (shift-expression-no-bf >> additive-expression #'build-binary-expression)
   (shift-expression-no-bf >>> additive-expression #'build-binary-expression))

  (relational-expression
   shift-expression
   (relational-expression < shift-expression #'build-binary-expression)
   (relational-expression > shift-expression #'build-binary-expression)
   (relational-expression <= shift-expression #'build-binary-expression)
   (relational-expression >= shift-expression #'build-binary-expression)
   (relational-expression instanceof shift-expression #'build-binary-expression)
   (relational-expression in shift-expression #'build-binary-expression))

  (relational-expression-no-in
   shift-expression
   (relational-expression-no-in < shift-expression #'build-binary-expression)
   (relational-expression-no-in > shift-expression #'build-binary-expression)
   (relational-expression-no-in <= shift-expression #'build-binary-expression)
   (relational-expression-no-in >= shift-expression #'build-binary-expression)
   (relational-expression-no-in instanceof shift-expression #'build-binary-expression))

  (relational-expression-no-bf
   shift-expression-no-bf
   (relational-expression-no-bf < shift-expression #'build-binary-expression)
   (relational-expression-no-bf > shift-expression #'build-binary-expression)
   (relational-expression-no-bf <= shift-expression #'build-binary-expression)
   (relational-expression-no-bf >= shift-expression #'build-binary-expression)
   (relational-expression-no-bf instanceof shift-expression #'build-binary-expression)
   (relational-expression-no-bf in shift-expression #'build-binary-expression))

  )
  
;;; Test function, read a valid javascript clip and return an AST.
(defun parse (code)
  (parse-with-lexer (jsimple-lexer code)
		    *jsimple-parser*))

;;;; NOTE:
;;;; Javascript syntax and grammar in ECMA-262:
;;;; | means OR, so char | will be \|, \ means EXCEPT-FOR, char \ will be \\,
;;;; [] will contain alternative choice (not primary, but tail),
;;;; :: means CAN-BE-VIEWED-AS, () will contain anything that is optional,
;;;; {} will contain descriptive scoping elements.
;;;; - without surrounding space means natural scope, e.g. 0-9 means 0 to 9.
;;;; all other characters evaluate to themselves.
;;;;
;;;; input-element-div ::
;;;;   whitespace | line-terminator | comment | common-token |
;;;;   div-punctuator |  right-brace-punctuator
;;;; whitespace ::
;;;;   TAB | VT | FF | SP | NBSP | ZWNBSP | USP
;;;; line-terminator ::
;;;;   LF | CR | LS | PS
;;;; comment ::
;;;;   multi-line-comment | single-line-comment
;;;; multi-line-comment ::
;;;;   /* (multi-line-comment-chars) */
;;;; multi-line-comment-chars ::
;;;;   multi-line-not-asterisk-char (multi-line-comment-chars) |
;;;;   * (post-asterisk-comment-chars)
;;;; post-asterisk-comment-chars ::
;;;;   multi-line-not-forward-slash-or-asterisk-char (multi-line-comment-chars) |
;;;;   * (post-asterisk-comment-chars)
;;;; multi-line-not-asterisk-char ::
;;;;   source-character \ *
;;;; multi-line-not-forward-slash-or-asterish-char ::
;;;;   source-character \ [* | /]
;;;; single-line-comment ::
;;;;   // (single-line-comment-chars)
;;;; single-line-comment-chars ::
;;;;   single-line-comment-char (single-line-comment-chars)
;;;; single-line-comment-char ::
;;;;   source-character \ line-terminator
;;;; common-token ::
;;;;   id | punctuator | number | string | template
;;;; identifier-name ::
;;;;   identifier-start | identifier-name identifier-part
;;;; identifier-start ::
;;;;   unicode-id-start | $ | _ | \\ unicode-escape-sequence
;;;; identifier-part ::
;;;;   unicode-id-continue | $ | _ | \\ unicode-escape-sequence | ZWNJ | ZWJ
;;;; unicode-id-start ::
;;;;   {any unicode with property ID_Start or Other_ID_Start}
;;;; unicode-id-continue ::
;;;;   {any unicode with property ID_Continue, Other_ID_Continue or
;;;;   Other_ID_Start}
;;;; reserved-word ::
;;;;   keyword | future-reserved-word | null-literal | boolean-literal
;;;; keyword ::
;;;;   break | do | in | typeof | case | else | instanceof | var | catch |
;;;;   export | new | void | class | extends | return | while | const |
;;;;   finally | super | with | continue | for | switch | yield | debugger |
;;;;   function | this | default | if | throw | delete | import | try
;;;; future-reserved-word ::
;;;;   enum | await
;;;; punctuator ::
;;;;   \{ | \} | \( | \) | \[ | \] | . | ; | , | < | > | <= | >= | == | != |
;;;;   === | !== | + | - | * | % | ++ | -- | << | >> | >>> | & | \| | ^ | ! |
;;;;   ~ | && | \|\| | ? | : | = | += | -= | *= | %= | <<= | >>= | >>>= | &= |
;;;;   |= | ^= | =>
;;;; div-punctuator ::
;;;;   / | /=
;;;; null-literal ::
;;;;   null
;;;; boolean-literal ::
;;;;   true | false
;;;; numeric-literal ::
;;;;   decimal-literal | binary-integer-literal | octal-integer-literal |
;;;;   hex-integer-literal
;;;; decimal-literal ::
;;;;   decimal-integer-literal . (decimal-digits) (exponent-part) |
;;;;   . decimal-digits (exponent-part) | decimal-integer-literal (exponent-part)
;;;; decimal-integer-literal ::
;;;;   0 | non-zero-digit (decimal-digits)
;;;; decimal-digits ::
;;;;   decimal-digit | decimal-digits decimal-digit
;;;; decimal-digit ::
;;;;   0-9
;;;; non-zero-digit ::
;;;;   1-9
;;;; exponent-part ::
;;;;   exponent-indicator signed-integer
;;;; exponent-indicator ::
;;;;   e | E
;;;; signed-integer ::
;;;;   decimal-digits | + decimal-digits | - decimal-digits
;;;; binary-integer-literal ::
;;;;   0b binary-digits | 0B binary-digits
;;;; binary-digits ::
;;;;   binary-digit | binary-digits binary-digit
;;;; binary-digit ::
;;;;   0 | 1
;;;; octal-integer-literal ::
;;;;   0o octal-digits | 0O octal-digits
;;;; octal-digits ::
;;;;   octal-digit | octal-digits octal-digit
;;;; octal-digit ::
;;;;   0-7
;;;; hex-integer-literal ::
;;;;   0x hex-digits | 0X hex-digits
;;;; hex-digits ::
;;;;   hex-digit | hex-digits hex-digit
;;;; hex-digit ::
;;;;   0-9 | a-f | A-F
;;;; string-literal ::
;;;;   " (double-string-characters) " | ' (single-string-characters) '
;;;; double-string-characters ::
;;;;   double-string-character (double-string-characters)
;;;; single-string-characters ::
;;;;   single-string-character (single-string-characters)
;;;; double-string-character ::
;;;;   source-character \ [" | \\ | line-terminator]
;;;;   \\ escape-sequence
;;;;   line-continuation
;;;; single-string-character ::
;;;;   source-character \ [' | \\ | line-terminator]
;;;;   \\ escape-sequence
;;;;   line-continuation
;;;; line-continuation ::
;;;;   \\ line-terminator-sequence
;;;; escape-sequence ::
;;;;   character-escape-sequence | 0 | hex-escape-sequence | unicode-escape-sequence
;;;; character-escape-sequence ::
;;;;   single-escape-character | non-escape-character
;;;; single-escape-character ::
;;;;   ' | " | \\ | b | f | n | r | t | v
;;;; non-escape-character ::
;;;;   source-character \ [escape-character | line-terminator]
;;;; escape-character ::
;;;;   single-escape-character | decimal-digit | x | u
;;;; hex-escape-sequence ::
;;;;   x hex-digit hex-digit
;;;; unicode-escape-sequence ::
;;;;   u hex-4-digits | u \{ hex-digits \}
;;;; hex-4-digits ::
;;;;   hex-digit hex-digit hex-digit hex-digit
;;;; regular-expression-literal ::
;;;;   / regular-expression-body / regular-expression-flags
;;;; regular-expression-body ::
;;;;   regular-expression-first-char regular-expression-chars
;;;; regular-expression-chars ::
;;;;   [] | regular-expression-chars regular-expression-char
;;;; regular-expression-first-char ::
;;;;   regular-expression-non-terminator \ [* | \\ | / | \[] |
;;;;   regular-expression-backslash-sequence | regular-expression-class
;;;; regular-expression-char ::
;;;;   regular-expression-non-terminator \ [\\ | / | \[] |
;;;;   regular-expression-backslash-sequence | regular-expression-class
;;;; regular-expression-backslash-sequence ::
;;;;   \\ regular-expression-non-terminator
;;;; regular-expression-non-terminator ::
;;;;   source-character \ line-terminator
;;;; regular-expression-class ::
;;;;   \[ regular-expression-class-chars \]
;;;; regular-expression-class-chars ::
;;;;   [] | regular-expression-class-chars regular-expression-class-char
;;;; regular-expression-class-char ::
;;;;   regular-expression-none-terminator \ [\] | \\]
;;;;   regular-expression-backslash-sequence
;;;; regular-expression-flags ::
;;;;   [] | regular-expression-flags identifier-part
;;;; template ::
;;;;   no-substitution-template | template-head
;;;; no-substitution-template ::
;;;;   ` (template-characters) `
;;;; template-head ::
;;;;   ` (template-characters) $ \{
;;;; template-substitution-tail ::
;;;;   template-middle | template-tail
;;;; template-middle ::
;;;;   \} (template-characters) $ \{
;;;; template-tail ::
;;;;   \} (template-characters) `
;;;; template-characters ::
;;;;   template-character (template-characters)
;;;; template-character ::
;;;;   $ | \\ escape-sequence | line-continuation | line-terminator-sequence |
;;;;   source-character \ [` | \\ | $ | line-terminator]
