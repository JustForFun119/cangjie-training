# cangjie-training

Train my 倉頡 (Cangjie)

# Thanks

- 倉頡新星 https://gholk.github.io/cjns/index.html
- https://www.hkcards.com/
- Cangjie character-to-radicals dictionary from https://github.com/rime/rime-cangjie (https://raw.githubusercontent.com/rime/rime-cangjie/master/cangjie5.dict.yaml)
- Popular usage of Chinese character from https://humanum.arts.cuhk.edu.hk (https://humanum.arts.cuhk.edu.hk/Lexis/lexi-can/faq.php?s=1)
- 倉頡之友 https://www.chinesecj.com for Cangjie dictionary
- Workbox for PWA support (https://developer.chrome.com/docs/workbox/modules/workbox-cli/)

# Build app

## Development

Build app scripts: `npx shadow-cljs watch app` or `npx shadow-cljs compile app`

## Release

1. Build app scripts: `npx shadow-cljs release app`
2. Update workbox config for PWA (offline etc.): `npx workbox-cli generateSW workbox-config.js`

# Why

To practise/learn 倉頡 (Cangjie) input 🀄
1. Practise Cangjie **radical input** using keyboard
  - Memorise radical-to-key mapping 記熟倉頡字母鍵位 (鍵盤指法)
    - ❔ I don't know which keyboard key to press for entering some Cangjie radical
    - 👨‍💻 practise typing with on-screen keyboard showing keys and Cangjie radicals
    - 倉頡字母 Cangjie radicals
      - 日月金木水火土 ABCDEFG
      - 竹戈十大中一弓 HIJKLMN (斜點交叉縱橫鉤)
      - 人心手口 OPQR
      - 尸廿山女田卜 STUVXY (側並仰紐方卜)
      - 難 X
  - Train radical extraction fluency 訓練取碼流暢度
    - ❔ I don't know/I am slow at breaking down a Chinese character into Cangjie radicals
    - 👨‍💻 practise by example: write Chinese character with/without radical hints
    - example: to write 2 characters "香港"
      - character 香 = radicals 竹 木 日 = keyboard keys "H" "D" "A"
      - 港 = 水 廿 金 山 = "E" "T" "C" "U"
2. Apply **spaced repetition learning** method for Cangjie training
  - [SM-2](https://www.supermemo.com/en/archives1990-2015/english/ol/sm2)
  - [modified SM-2](https://www.blueraja.com/blog/477/a-better-spaced-repetition-learning-algorithm-sm2) ([implemented here](src\main\cangjie_training\learner.cljs))