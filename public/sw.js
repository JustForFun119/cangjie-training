if(!self.define){let e,s={};const r=(r,c)=>(r=new URL(r+".js",c).href,s[r]||new Promise((s=>{if("document"in self){const e=document.createElement("script");e.src=r,e.onload=s,document.head.appendChild(e)}else e=r,importScripts(r),s()})).then((()=>{let e=s[r];if(!e)throw new Error(`Module ${r} didn’t register its module`);return e})));self.define=(c,i)=>{const o=e||("document"in self?document.currentScript.src:"")||location.href;if(s[o])return;let j={};const n=e=>r(e,o),l={module:{uri:o},exports:j,require:n};s[o]=Promise.all(c.map((e=>l[e]||n(e)))).then((e=>(i(...e),j)))}}define(["./workbox-02fe9890"],(function(e){"use strict";self.addEventListener("message",(e=>{e.data&&"SKIP_WAITING"===e.data.type&&self.skipWaiting()})),e.precacheAndRoute([{url:"index.html",revision:"fb80a637e3a91a63b8da9688415c20b9"},{url:"js/cljs-runtime/cangjie_training.app.js",revision:"f02d7514b7e7c3f190cd9a58fbb5c3ab"},{url:"js/cljs-runtime/cangjie_training.dictionary.js",revision:"5acb794b35d18ea0573b017fea6fcb6a"},{url:"js/cljs-runtime/cangjie_training.event_fx.js",revision:"a7e72de6a640c134146a93d65b63eb93"},{url:"js/cljs-runtime/cangjie_training.learner.js",revision:"21d7fbc67baba0b581e7a8e56ad53d0a"},{url:"js/cljs-runtime/cangjie_training.model.js",revision:"7a4a4364e6e81fe5dfa4843c2f75a9fe"},{url:"js/cljs-runtime/cangjie_training.ui.js",revision:"5c197ecd619ac201b98083eeab3b0ea1"},{url:"js/cljs-runtime/cangjie_training.util.js",revision:"3184ca67626d6903857bc3a33287c107"},{url:"js/cljs-runtime/ciangje_training.app.js",revision:"58f15e3b8576c511ce684efec42b203b"},{url:"js/cljs-runtime/ciangjie_training.app.js",revision:"f5e19195db7394cec3748f0fc38da175"},{url:"js/cljs-runtime/cider.nrepl.inlined_deps.suitable.v0v4v1.suitable.js_introspection.js",revision:"2d351d3a790ba6ff32057b5472f1a081"},{url:"js/cljs-runtime/cljs.core.async.impl.buffers.js",revision:"dd42b8254ca3dca88aebabf8fcd07cd4"},{url:"js/cljs-runtime/cljs.core.async.impl.channels.js",revision:"672736de8d2685c595f2786d83c933e4"},{url:"js/cljs-runtime/cljs.core.async.impl.dispatch.js",revision:"7c49883ee57b60d31019c3d54eb7b10f"},{url:"js/cljs-runtime/cljs.core.async.impl.ioc_helpers.js",revision:"cfeed1f57313b7892a319ba75ea53e5c"},{url:"js/cljs-runtime/cljs.core.async.impl.protocols.js",revision:"56440e8e9cfcb3c39c618734ca01cf9e"},{url:"js/cljs-runtime/cljs.core.async.impl.timers.js",revision:"d1376ce1ca3f8ffc2a8b40d7205d9296"},{url:"js/cljs-runtime/cljs.core.async.js",revision:"d4c20e497ccac2e6539a5433c8d1c13c"},{url:"js/cljs-runtime/cljs.core.js",revision:"c8c7d9acce36d88ab442613ba02e0431"},{url:"js/cljs-runtime/cljs.pprint.js",revision:"813410af67c05cdf5610bee6839ed1f2"},{url:"js/cljs-runtime/cljs.repl.js",revision:"96d81f4108b6ea60a4c2cf2e4420b3ec"},{url:"js/cljs-runtime/cljs.spec.alpha.js",revision:"4eeb973db8cdd1b405923e50df17ab09"},{url:"js/cljs-runtime/cljs.spec.gen.alpha.js",revision:"bb1061141744408fefd32d581c7789dd"},{url:"js/cljs-runtime/cljs.tools.reader.impl.commons.js",revision:"85ec7f0daf732262ae89fec8d5d00e75"},{url:"js/cljs-runtime/cljs.tools.reader.impl.errors.js",revision:"36d6a9d70711bae16c5c2d488e7c84ee"},{url:"js/cljs-runtime/cljs.tools.reader.impl.inspect.js",revision:"8d63a21b6c0c074b1419574599cc9eaf"},{url:"js/cljs-runtime/cljs.tools.reader.impl.utils.js",revision:"35724a320f976adeba04816a120b1126"},{url:"js/cljs-runtime/cljs.tools.reader.js",revision:"32781bdb97b82f7c3b98953ac9a83420"},{url:"js/cljs-runtime/cljs.tools.reader.reader_types.js",revision:"11e2f875b5724930ac3c5881ba5fbff6"},{url:"js/cljs-runtime/cljs.user.js",revision:"699e569e31afe7e68341fd016c8cc067"},{url:"js/cljs-runtime/cljsjs.react.dom.js",revision:"6af411520d9a606f4964d0c42efe66f9"},{url:"js/cljs-runtime/cljsjs.react.js",revision:"6c631d38d8807f7579fc8b1fb41697b1"},{url:"js/cljs-runtime/clojure.core.protocols.js",revision:"9fdb1940b087b41f8a28ce2047a09be3"},{url:"js/cljs-runtime/clojure.data.js",revision:"77a55bcb0f8f84c9a06c37b36fd28a0d"},{url:"js/cljs-runtime/clojure.datafy.js",revision:"2af79fdceb04c24638535915890b53c9"},{url:"js/cljs-runtime/clojure.set.js",revision:"a0074049b1b288be60db613d5c24349c"},{url:"js/cljs-runtime/clojure.string.js",revision:"7f0642c78e41ecc50375610d02ab8342"},{url:"js/cljs-runtime/clojure.walk.js",revision:"8ecf06617eec076406727554d5b8b99d"},{url:"js/cljs-runtime/cognitect.transit.js",revision:"714771a42977a4f99b5ca9b6790073b3"},{url:"js/cljs-runtime/com.cognitect.transit.caching.js",revision:"3d947905abdc36d8e4bed88b2c05cf98"},{url:"js/cljs-runtime/com.cognitect.transit.delimiters.js",revision:"7d538f5633c1d9fdfbdd114be59bbfa9"},{url:"js/cljs-runtime/com.cognitect.transit.eq.js",revision:"e47d60a50f6b767d114ba6d7a2952f77"},{url:"js/cljs-runtime/com.cognitect.transit.handlers.js",revision:"bc878ab8c5eacbf905e12a0353b94921"},{url:"js/cljs-runtime/com.cognitect.transit.impl.decoder.js",revision:"136171d5921dc38c21c95ee90ea06391"},{url:"js/cljs-runtime/com.cognitect.transit.impl.reader.js",revision:"f850e2ee7da535134350b9bb5414afb0"},{url:"js/cljs-runtime/com.cognitect.transit.impl.writer.js",revision:"832d197a6e2fecaf5e95cdc8cd7f9e5d"},{url:"js/cljs-runtime/com.cognitect.transit.js",revision:"cc29b6ebcad5e152778017d0205519f1"},{url:"js/cljs-runtime/com.cognitect.transit.types.js",revision:"f3382c0ce4f0ffb045d595b45d8aadde"},{url:"js/cljs-runtime/com.cognitect.transit.util.js",revision:"80a23ae2868f3bd4597b9152cd0d0c11"},{url:"js/cljs-runtime/daiquiri.core.js",revision:"53f57dd4bfc89acf15827950598463b5"},{url:"js/cljs-runtime/daiquiri.interpreter.js",revision:"c2c1b394b2f4b407f9eaac76ae359961"},{url:"js/cljs-runtime/daiquiri.normalize.js",revision:"67e7cac767bd1ca4e11a2223766ff97e"},{url:"js/cljs-runtime/daiquiri.util.js",revision:"6c6822acda1642349977e62fc0e72c7a"},{url:"js/cljs-runtime/goog.array.array.js",revision:"fe7d087e5219a2d6cd06ae0f26a81298"},{url:"js/cljs-runtime/goog.asserts.asserts.js",revision:"4e2fe3f2c10418f3c88eac5c5075e287"},{url:"js/cljs-runtime/goog.async.nexttick.js",revision:"db66c3ae7ab3d1c2911d7510af6fb6bd"},{url:"js/cljs-runtime/goog.base.js",revision:"7d801096ecefcd423016bc0ff5891c08"},{url:"js/cljs-runtime/goog.collections.iters.js",revision:"8c38949bdd2f7e96dfe86f0ca8592749"},{url:"js/cljs-runtime/goog.collections.maps.js",revision:"39a98c310507ce48a1350425149125b6"},{url:"js/cljs-runtime/goog.debug.debug.js",revision:"ec6535477975b7b83b2d3db157af06c3"},{url:"js/cljs-runtime/goog.debug.entrypointregistry.js",revision:"c3f83f0f5e4e803eda5264c112269039"},{url:"js/cljs-runtime/goog.debug.error.js",revision:"ec41ea08cd968e56536262a601e18e99"},{url:"js/cljs-runtime/goog.debug.errorcontext.js",revision:"c2d609e24fc146dae2aa899d21abd910"},{url:"js/cljs-runtime/goog.disposable.disposable.js",revision:"544217e96728d5db3a3f35373db8f810"},{url:"js/cljs-runtime/goog.disposable.dispose.js",revision:"b8821e809f330e26f6919041ddf19990"},{url:"js/cljs-runtime/goog.disposable.disposeall.js",revision:"e4a83b79375a4aaf5a2a7ef507d7e775"},{url:"js/cljs-runtime/goog.disposable.idisposable.js",revision:"abb829d0535e311765cd3b5f8af7b327"},{url:"js/cljs-runtime/goog.dom.asserts.js",revision:"31fa1f35bdd86a6e0cdd3c506a58accb"},{url:"js/cljs-runtime/goog.dom.browserfeature.js",revision:"c30b5abe7ad4ac1d2abd21024358de59"},{url:"js/cljs-runtime/goog.dom.classlist.js",revision:"5b433b5e4535917508dbc1cb37f65d3a"},{url:"js/cljs-runtime/goog.dom.dom.js",revision:"00c27a078975c0165c9033447031145e"},{url:"js/cljs-runtime/goog.dom.forms.js",revision:"dc78963747c2c4209ec62556165c851e"},{url:"js/cljs-runtime/goog.dom.htmlelement.js",revision:"632967b576298f619055323019ea3920"},{url:"js/cljs-runtime/goog.dom.inputtype.js",revision:"614cce5cb34faddc4fe6ba68533ee25b"},{url:"js/cljs-runtime/goog.dom.nodetype.js",revision:"e4ed755fadb4b1b451db3f8b1dae370c"},{url:"js/cljs-runtime/goog.dom.safe.js",revision:"656cf48c18c48f5b523f072b032bc0af"},{url:"js/cljs-runtime/goog.dom.tagname.js",revision:"3d2ca1445affdba27977c0d199403c25"},{url:"js/cljs-runtime/goog.dom.tags.js",revision:"855bf0f0ee649d50b818176daa9996a0"},{url:"js/cljs-runtime/goog.dom.vendor.js",revision:"812a25fa65890fa590c0500a7977d279"},{url:"js/cljs-runtime/goog.events.event.js",revision:"51cb76dcdac3ed62ac1d22ce0cf988cf"},{url:"js/cljs-runtime/goog.events.eventid.js",revision:"d71a2f11f1286cb67517050ea03d0229"},{url:"js/cljs-runtime/goog.fs.blob.js",revision:"240496017ec7274aa1f56f303b07b114"},{url:"js/cljs-runtime/goog.fs.url.js",revision:"88899e844e66cd9d70afb6e18c3a8b85"},{url:"js/cljs-runtime/goog.functions.functions.js",revision:"9e18b40043a38c7ed2716839ec9a930f"},{url:"js/cljs-runtime/goog.html.safehtml.js",revision:"46c4641c3249ba087a851a2309e03b73"},{url:"js/cljs-runtime/goog.html.safescript.js",revision:"60341964895d5cfb865a06aa2c3da914"},{url:"js/cljs-runtime/goog.html.safestyle.js",revision:"53c6483290509b476a07138b1867f226"},{url:"js/cljs-runtime/goog.html.safestylesheet.js",revision:"584aab99a85b49822546be195fbf89e1"},{url:"js/cljs-runtime/goog.html.safeurl.js",revision:"b7cf094447088fc07628148c84d52bfc"},{url:"js/cljs-runtime/goog.html.trustedresourceurl.js",revision:"2b987136abab73b68d3942e7885bf8ed"},{url:"js/cljs-runtime/goog.html.trustedtypes.js",revision:"e00dc8f955dd4f33248c1adb4f0794d1"},{url:"js/cljs-runtime/goog.html.uncheckedconversions.js",revision:"bec7a5383919c46f5848bafb2a101982"},{url:"js/cljs-runtime/goog.i18n.bidi.js",revision:"283b356c7e3e4b1868c55ed82dcb57c8"},{url:"js/cljs-runtime/goog.iter.es6.js",revision:"015b94b066b2e7f73ef1443397a60f90"},{url:"js/cljs-runtime/goog.iter.iter.js",revision:"f7195c25f016c53592f4a8cda69db2d1"},{url:"js/cljs-runtime/goog.labs.useragent.browser.js",revision:"f9b652238b64ca3a5898b09c3cc17945"},{url:"js/cljs-runtime/goog.labs.useragent.engine.js",revision:"b0a7efe357a1106c2eca8f2ffb7b28c4"},{url:"js/cljs-runtime/goog.labs.useragent.platform.js",revision:"4fe2899e4edbee5155ad42d90a2a2bad"},{url:"js/cljs-runtime/goog.labs.useragent.useragent.js",revision:"e8b9e5980f64c9e018fa755559160013"},{url:"js/cljs-runtime/goog.labs.useragent.util.js",revision:"58c8351209cca96ca9d0bd43696edb86"},{url:"js/cljs-runtime/goog.math.box.js",revision:"b129928d944b40adac1d7b60c25d1c93"},{url:"js/cljs-runtime/goog.math.coordinate.js",revision:"c20f63e75b3f0dc86da130d31991aade"},{url:"js/cljs-runtime/goog.math.integer.js",revision:"b66373bd7f7c2947a81a0595329e7ec3"},{url:"js/cljs-runtime/goog.math.irect.js",revision:"bb87708b21a6cbdb54e204725af83fb1"},{url:"js/cljs-runtime/goog.math.long.js",revision:"f53dc59cf48e6257582415121b52d009"},{url:"js/cljs-runtime/goog.math.math.js",revision:"986f0bf9881753724a88297f68e6bff7"},{url:"js/cljs-runtime/goog.math.rect.js",revision:"7d1bafc4712ebc8db59eb2d4c8d6e0ec"},{url:"js/cljs-runtime/goog.math.size.js",revision:"fe4fd63fbfcf0f07e0524b25d5963adb"},{url:"js/cljs-runtime/goog.object.object.js",revision:"564861cf474d7846e1dcd4fd4cab8c72"},{url:"js/cljs-runtime/goog.reflect.reflect.js",revision:"d09ba20ef067d277261a3c60311eb65b"},{url:"js/cljs-runtime/goog.string.const.js",revision:"15bd07158d26da959cd8baecb61a1598"},{url:"js/cljs-runtime/goog.string.internal.js",revision:"578605e7669b55d1b2bc8d2624c07752"},{url:"js/cljs-runtime/goog.string.string.js",revision:"6bed72bdd5b165eedaea57fc0e651c6b"},{url:"js/cljs-runtime/goog.string.stringbuffer.js",revision:"cf3dafe1ae2f8597d7b9c55c7610a945"},{url:"js/cljs-runtime/goog.string.stringformat.js",revision:"7a58c1ae998e8588c217cb0e2e181d31"},{url:"js/cljs-runtime/goog.string.typedstring.js",revision:"75a105f6f3b5d68270c0980f8adaadf3"},{url:"js/cljs-runtime/goog.structs.map.js",revision:"2194ba35f9fe50a0989b01b291ca9b3f"},{url:"js/cljs-runtime/goog.structs.structs.js",revision:"b5ef5cd7483d1afb023b04b530e357ca"},{url:"js/cljs-runtime/goog.style.style.js",revision:"da630b2a1b0685ac16d0c2b9317c8f84"},{url:"js/cljs-runtime/goog.style.transition.js",revision:"d86c12976074e795fdba2065cc741c95"},{url:"js/cljs-runtime/goog.uri.uri.js",revision:"541c3f2bf874e045283a380624dd3921"},{url:"js/cljs-runtime/goog.uri.utils.js",revision:"3d5f354662094787c393d5b3fd9d84fc"},{url:"js/cljs-runtime/goog.useragent.product.js",revision:"39c2a4221b3cfe8419be7982435a2c81"},{url:"js/cljs-runtime/goog.useragent.useragent.js",revision:"60b233564c67e06813e5c52cdf46de34"},{url:"js/cljs-runtime/goog.window.window.js",revision:"342a1392968e31e61ebc36afb49a817d"},{url:"js/cljs-runtime/module$node_modules$object_assign$index.js",revision:"ab7ae63240b089cf90d1f1034fca0481"},{url:"js/cljs-runtime/module$node_modules$prop_types$checkPropTypes.js",revision:"8b894e4a1090a0af2674fe96f4ecf582"},{url:"js/cljs-runtime/module$node_modules$prop_types$lib$has.js",revision:"bb69a6a8b17c107cab4744524dc24d76"},{url:"js/cljs-runtime/module$node_modules$prop_types$lib$ReactPropTypesSecret.js",revision:"30b8708d146cbbce2f6768a31ebebafb"},{url:"js/cljs-runtime/module$node_modules$react_dom$cjs$react_dom_development.js",revision:"5ef4a705a0d3d2ed51c0d67cb5136049"},{url:"js/cljs-runtime/module$node_modules$react_dom$cjs$react_dom_production_min.js",revision:"c24a4916040e3b797cfba4f1a7fb2ee0"},{url:"js/cljs-runtime/module$node_modules$react_dom$index.js",revision:"c65400519b4d458069b98f5cd8b3d7f9"},{url:"js/cljs-runtime/module$node_modules$react$cjs$react_development.js",revision:"17ebc49a7bacef7c68c568dbc16150a1"},{url:"js/cljs-runtime/module$node_modules$react$cjs$react_production_min.js",revision:"8b994b50fe6efcc21a62f4a613c4cb1f"},{url:"js/cljs-runtime/module$node_modules$react$index.js",revision:"4bbaccab689c1a23aeec6c6b4107b24d"},{url:"js/cljs-runtime/module$node_modules$scheduler$cjs$scheduler_development.js",revision:"02f0c2a8985c7589003810b9e44cdc95"},{url:"js/cljs-runtime/module$node_modules$scheduler$cjs$scheduler_production_min.js",revision:"ace28a5b48229b459c4100757bf9be62"},{url:"js/cljs-runtime/module$node_modules$scheduler$cjs$scheduler_tracing_development.js",revision:"8229168d5b6d5bdcd51d0e142b8bf277"},{url:"js/cljs-runtime/module$node_modules$scheduler$cjs$scheduler_tracing_production_min.js",revision:"2d5ecb5c9cf89b84a7914464ff6382af"},{url:"js/cljs-runtime/module$node_modules$scheduler$index.js",revision:"0646653e7d99b4c07fccdefc6cc6a317"},{url:"js/cljs-runtime/module$node_modules$scheduler$tracing.js",revision:"80ebd531c90a454ff9a580a04f383fd6"},{url:"js/cljs-runtime/reagent.core.js",revision:"20a7e4a8a2f643a1a537fbbf6781ebbc"},{url:"js/cljs-runtime/reagent.debug.js",revision:"6bc17d3050bdfc8490a568b24e1c0119"},{url:"js/cljs-runtime/reagent.dom.js",revision:"d0b22893f7ff9991eef0dd5c48294e87"},{url:"js/cljs-runtime/reagent.impl.batching.js",revision:"e7fb3e0804bbd41e5caeda38ce444359"},{url:"js/cljs-runtime/reagent.impl.component.js",revision:"3f4dd8bbce333400fe5469f23b8dec1e"},{url:"js/cljs-runtime/reagent.impl.template.js",revision:"b8b5cd4575e6547100b6f285c1ffe04f"},{url:"js/cljs-runtime/reagent.impl.util.js",revision:"f8425f5d90f68ec6ab2edc71a7d7107b"},{url:"js/cljs-runtime/reagent.ratom.js",revision:"0f75f17edf09aad2ec7d5d1d6dc70689"},{url:"js/cljs-runtime/rum.core.js",revision:"36084b6715ba057f1e32c6d7552876bc"},{url:"js/cljs-runtime/rum.cursor.js",revision:"78d4041621c0b24f37a1629e64eed28d"},{url:"js/cljs-runtime/rum.derived_atom.js",revision:"88321453ff5168910a4e0dd586d71426"},{url:"js/cljs-runtime/rum.specs.js",revision:"e700f00fa18809ca3e17b265992ca085"},{url:"js/cljs-runtime/rum.util.js",revision:"3d8d366a4b2bc9a5f128265f1f077e48"},{url:"js/cljs-runtime/shadow.animate.js",revision:"a719c554577b996b0d4854e1e8cea5f7"},{url:"js/cljs-runtime/shadow.cljs.devtools.client.browser.js",revision:"d1ed2109b8c483ce5b61521f10d9fa80"},{url:"js/cljs-runtime/shadow.cljs.devtools.client.console.js",revision:"992ea13c9bce2530d23fdaa0536e188b"},{url:"js/cljs-runtime/shadow.cljs.devtools.client.env.js",revision:"ac8c943a280eb04c482f7fc3353594d3"},{url:"js/cljs-runtime/shadow.cljs.devtools.client.hud.js",revision:"82257f202e2f91d95e9dc80ad65e334e"},{url:"js/cljs-runtime/shadow.cljs.devtools.client.shared.js",revision:"208bc89bee65d6d7c86be24929802f03"},{url:"js/cljs-runtime/shadow.cljs.devtools.client.websocket.js",revision:"a3518c1ae0207004e36bb1fad59c8271"},{url:"js/cljs-runtime/shadow.dom.js",revision:"008039873629273b8791b3e4406acba6"},{url:"js/cljs-runtime/shadow.js.js",revision:"366342a168662a20d3d746e6beb1b7a2"},{url:"js/cljs-runtime/shadow.json.js",revision:"cf45c15f0c884c3410327078aee16fed"},{url:"js/cljs-runtime/shadow.module.main.append.js",revision:"c5c8526e1592820a1c0070147daee663"},{url:"js/cljs-runtime/shadow.object.js",revision:"39963e73ebe6ccab0b90b28525c6d241"},{url:"js/cljs-runtime/shadow.remote.runtime.api.js",revision:"9495c260101c07297bca62b8f7588d2e"},{url:"js/cljs-runtime/shadow.remote.runtime.cljs.js_builtins.js",revision:"5c5b75a7c3570d622869a032ea999380"},{url:"js/cljs-runtime/shadow.remote.runtime.eval_support.js",revision:"1d20c73acf86b42f600639cbbce494bc"},{url:"js/cljs-runtime/shadow.remote.runtime.obj_support.js",revision:"b8421fc40d3a35fed1a0ddbf9bc98c51"},{url:"js/cljs-runtime/shadow.remote.runtime.shared.js",revision:"d1089fe3b6c3a2c16f3ea906cea90980"},{url:"js/cljs-runtime/shadow.remote.runtime.tap_support.js",revision:"1eb2ab080e3d78eabc5bab23a17da7cf"},{url:"js/cljs-runtime/shadow.remote.runtime.writer.js",revision:"3ee18af9179be0ee827cc473b3631643"},{url:"js/cljs-runtime/shadow.util.js",revision:"32d30a53010efcc1f822ebe1824f1550"},{url:"js/main.js",revision:"e13696993b8df7c8d4cadd8320045916"},{url:"js/tailwindcss_3.1.8.js",revision:"75d8e947aa3c574907e1dde74563cd11"}],{ignoreURLParametersMatching:[/^utm_/,/^fbclid$/]})}));
//# sourceMappingURL=sw.js.map
