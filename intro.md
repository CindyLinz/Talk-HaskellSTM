## 主題簡介

開發一個 multi-thread, 善用多核 CPU 運算資源, 而且彼此合作機制複雜, 而又沒有錯誤的程式是一個挑戰. 可以利用的工具很多, 建議的 design pattern 們很多, 而所選擇的工具或 design pattern 帶來的問題也很多...

STM (Software Transactional Memory) 是眾工具中的一個, 讓使用者以類似資料庫 transaction 的角度來運用它, 創造一個 atomic 的子空間, 提供 commit, abort/rollback, retry 等等用法, 讓使用者以比較高階的角度來組織並理解自己程式的正確性.

這次除了簡單介紹一下 STM 的用法之外, 也會看一看 STM 的實作. 有實作 STM 的程式語言很多, 這次是以 Haskell 的 GHC 實作裡的 STM 作為例子 (以 C 實作, 給 Haskell 用), 一起來看一下 STM 的實作方式.

## 講者簡介

我是 Gamesofa (神來也) 的工程師, 工作內容是開發遊戲後端會用到的東西們. 我喜歡的程式語言是 Haskell, Perl, 與 C. 另外, 我也很喜歡數學, 邏輯電路, 還有一些機械原理.

而最近兩年我對政治學與經濟學, 比較切身的部分也非常有興趣, 花很多時間在補充知識...

在工作之餘, 我正在進行一個 [自造 Haskell compiler 的小組活動](https://github.com/CindyLinz/BYOHC-Workshop), 玩一點點自造小機械, 還有跟朋友們研究討論政治經濟的東西 (像是 [最近寫的文章](https://github.com/CindyLinz/Article/blob/master/2016.1.7-%E8%BA%AB%E7%82%BA%E4%B8%80%E5%80%8B%E4%B8%AD%E5%9C%8B%E4%BA%BA%EF%BC%8C%E5%8F%AF%E4%BB%A5%E5%A6%82%E4%BD%95%E5%8F%8D%E5%B0%8D%E4%B8%8D%E8%AA%8D%E5%90%8C%E7%9A%84%E6%94%BF%E7%AD%96.md))....
