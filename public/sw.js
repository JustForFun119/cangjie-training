if(!self.define){let e,i={};const s=(s,t)=>(s=new URL(s+".js",t).href,i[s]||new Promise((i=>{if("document"in self){const e=document.createElement("script");e.src=s,e.onload=i,document.head.appendChild(e)}else e=s,importScripts(s),i()})).then((()=>{let e=i[s];if(!e)throw new Error(`Module ${s} didn’t register its module`);return e})));self.define=(t,r)=>{const n=e||("document"in self?document.currentScript.src:"")||location.href;if(i[n])return;let o={};const c=e=>s(e,n),d={module:{uri:n},exports:o,require:c};i[n]=Promise.all(t.map((e=>d[e]||c(e)))).then((e=>(r(...e),o)))}}define(["./workbox-02fe9890"],(function(e){"use strict";self.addEventListener("message",(e=>{e.data&&"SKIP_WAITING"===e.data.type&&self.skipWaiting()})),e.precacheAndRoute([{url:"index.html",revision:"bb973c2f4ab39ae80f1065fcfddbde7f"},{url:"js/main.js",revision:"52284045bb79e509502f0c5de7bc69ef"},{url:"js/tailwindcss_3.1.8.js",revision:"75d8e947aa3c574907e1dde74563cd11"}],{ignoreURLParametersMatching:[/^utm_/,/^fbclid$/]})}));
//# sourceMappingURL=sw.js.map
