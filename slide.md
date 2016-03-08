class: inverse, center, middle

# S T M
# (Software Transactional Memory)
# 應用與實作

## CindyLinz

2016.3.8

---

## 一個編造的問題例子

  + 這是一個線上麻將遊戲的 server

--

  + 每個玩家由一個 player worker thread 服務

--

  + 每一局牌桌由一個 room worker thread 服務

--

  + 每個玩家依據過去遊戲記錄有個能力指標 (類似 ELO 的東西), 範圍 1000~3000

--

  + 成局開打的牌局裡, 玩家能力指標上下要在 50 以內 (能力相近比較好玩)

--

  + 玩家登入後, player worker 會把這個玩家丟進一個「配桌池」裡

    有空的 room worker 會去池裡撈出 (搶出) 符合條件的玩家們出來湊成一桌

--

```haskell
module Player where

data Player = Player
  { account :: String
  , score :: Int
  }

...
```

---

## Spin-lock / polling 法

  + Haskell standard library 裡有個類似 mutex 的 `MVar a`
    ```haskell
    data MVar a

    newEmptyMVar :: IO (MVar a)
    newMVar :: a -> IO (MVar a)

    takeMVar :: MVar a -> IO a -- 沒東西的時候就卡在那邊等
    putMVar :: MVar a -> a -> IO () -- 已經有東西的時候卡在那邊等

    tryTakeMVar :: MVar a -> IO (Maybe a)
    tryPutMVar :: MVar a -> a -> IO Bool

    modifyMVar :: MVar a -> (a -> IO (a, b)) -> IO b
    modifyMVar_ :: MVar a -> (a -> IO a) -> IO ()

    ...
    ```

    ```haskell
    data Maybe a = Nothing | Just a
    ```

---

## Spin-lock / polling 法

  + Polling / spin lock
    ```haskell
    main = do
      matchPool <- newMVar [] :: IO (MVar [Player])
      roomThreads <- replicateM 100 $ forkIO (roomMain matchPool)
        -- 同時最多開 100 個房間
      playerThreads <- replicateM 500 $ forkIO (playerMain matchPool)
        -- 同時最多 500 玩家登入
    ```

    <small>(註</small>
    ```haskell
    ($) :: (a -> b) -> a -> b
    f $ x = f x
    ```

---

## Spin-lock / polling 法

  + spin-lock / Polling
    ```haskell
    roomMain :: MVar [Player] -> IO ()
    roomMain matchPool = forever $ do
      let
        takeRoomPlayer = do
          poolPlayers <- takeMVar matchPool
          case ...檢查有沒有符合條件的組合... of
            Just (takenPlayers, poolPlayers') -> do
              putMVar matchPool poolPlayers'
              pure takenPlayers
            Nothing -> do
              putMVar matchPool poolPlayers
              threadDelay 10000 -- 看要不要休息一下 (10000 微秒)
              takeRoomPlayer

      roomPlayers <- takeRoomPlayer

      ... 開始玩..

    ```

---

## Spin-lock / polling 法

  + 邏輯易懂, 不容易寫錯

--

  + 不愛地球.. 空轉的時候蠻浪費資源的..

--

  + 一次鎖住整個 pool 有時候是效能瓶頸..

    一個 `roomThread` 在檢查 `matchPool` 有沒有想要的組合的時候, 別的 `roomThread` 都不能碰 `matchPool`

---

## 實作某種 wait queue..

```haskell
roomMain :: MVar [Player] -> MVar () -> IO ()
roomMain matchPool waitingRoom = forever $ do
  let
    takeRoomPlayer = do
      poolPlayers <- takeMVar matchPool
      case ...檢查有沒有符合條件的組合... of
        Just (takenPlayers, poolPlayers') -> do
          putMVar matchPool poolPlayers'
          tryPutMVar waitingRoom () -- 繼續叫別人起床
          pure takenPlayers
        Nothing -> do
          putMVar matchPool poolPlayers
          takeMVar waitingRoom -- 休息到有人叫我起床
          takeRoomPlayer

```

--

  + 解決空轉問題

--

  + 這程式不太好檢查, 一眼看去有錯也不容易看出來... 加註解也沒救

--

  + 有時候有錯的程式, 在測試的時候總是巧妙地正確了.....

--

  + Hiesenbug 的巢穴

---

## Non-deterministic multi-thread 程式

  + 如果有錯, 偶爾出錯

--

  + 正確性有一部分是在時間軸上, 從程式碼直接看不明顯

--

  + 測試無效

--

  + 有一系列的邏輯理論系統用來處理這種問題, 叫作 Temporal Logic

      - Interval temporal logic (ITL)
      - Linear temporal logic (LTL)
      - Computational tree logic (CTL)
      - Property specification language (PSL)
      - CTL* which generalizes LTL and CTL
      - Hennessy-Milner logic (HML)
      - μ calculus which includes as a subset HML and CTL*
      - Metric interval temporal logic (MITL)
      - Signal temporal logic (STL)

    理論模型多, 各有勝場, 表示這個問題很麻煩..

---

## STM 提供的功能

```haskell
atomically :: STM a -> IO a

newTVar :: a -> STM (TVar a)
readTVar :: TVar a -> STM a
writeTVar :: TVar a -> a -> STM ()

newTMVar :: a -> STM (TMVar a)
newEmptyTMVar :: STM (TMVar a)
takeTMVar :: TMVar a -> STM a -- 卡住的時候用 retry
putTMVar :: TMVar a -> a -> STM () -- 卡住的時候用 retry

throwSTM :: Exception e => e -> STM a
catchSTM :: Exception e => STM a -> (e -> STM a) -> STM a

retry :: STM a
orElse :: STM a -> STM a -> STM a

always :: STM Bool -> STM () -- 設立 invariant
alwaysSucceeds :: STM a -> STM () -- 設立 invariant

...
```

---

## 使用 STM 的例子

```haskell
main = do
  matchPool <- atomically (newTVar []) :: IO (TVar [Player])
  ...

roomMain :: TVar [Player] -> IO ()
roomMain matchPool = forever $ do
  roomPlayers <- atomically $ do
    poolPlayers <- readTVar matchPool
    case ...檢查有沒有符合條件的組合... of
      Just (takenPlayers, poolPlayers') -> do
        writeTVar poolPlayers'
        pure takenPlayers
      Nothing ->
        retry
  ...
```

---

## 示範 always / alwaysSucceeds (invariant) 用法

```haskell
data BigamyException = BigamyException deriving (Show, Typeable)
instance Exception BigamyException where

data Person = Person
  { name :: String
  , age :: Int
  , mate :: Maybe (TVar Person)
  }

marry :: TVar Person -> TVar Person -> IO ()
marry a b = atomically $ do
  aStatus <- readTVar a
  bStatus <- readTVar b
  forM_ [aStatus, bStatus] $ \st ->
    case mate st of
      Nothing -> pure ()
      Just _ -> throwSTM BigamyException
  writeTVar a aStatus{ mate = Just b }
  writeTVar b bStatus{ mate = Just a }
```

---

## 示範 always / alwaysSucceeds (invariant) 用法

```haskell
data AnimalException = AnimalException deriving (Show, Typeable)
instance Exception AnimalException where

allocatePerson :: String -> Int -> IO (TVar Person)
allocatePerson name age = atomically $ do
  me <- newTVar (Person name age Nothing)
  alwaysSucceeds $ do
    Person _ myAge myMate <- readTVar me
    case myMate of
      Nothing -> pure ()
      Just _ | myAge >= 16 -> pure ()
             | otherwise -> throwSTM AnimalException
  pure me

main = do
  girl <- allocatePerson "loli" 9
  uncle <- allocatePerson "s3p" 30
  marry uncle girl
```

---

## 示範 always / alwaysSucceeds (invariant) 用法

```haskell
allocatePerson :: String -> Int -> IO (TVar Person)
allocatePerson name age = atomically $ do
  me <- newTVar (Person name age Nothing)
  alwaysSucceeds $ do
    Person _ myAge myMate <- readTVar me
    case myMate of
      Nothing -> pure ()
      Just _ | myAge >= 16 -> pure ()
             | otherwise -> retry
  pure me

growUp :: TVar Person -> IO ()
growUp p = do
  ageFrom <- atomically $ do
    myStatus <- readTVar p
    writeTVar p myStatus{ age = age myStatus + 1 }
    pure (age myStatus)
  putStrLn $ "grow from " ++ show ageFrom

main = do
  girl <- allocatePerson "loli" 9
  forkIO $ forever $ do
    threadDelay 300000
    growUp girl

  uncle <- allocatePerson "s3p" 30
  marry uncle girl
```

---

## STM 執行時的資料結構

<embed src=struct-view.svg></embed>

---

## TVar

```c
typedef struct {
  StgHeader                  header;
  StgClosure                *volatile current_value;
  StgTVarWatchQueue         *volatile first_watch_queue_entry;
    // 這是 list of StgAtomicInvariant 或 Thread(StgTSO)
  StgInt                     volatile num_updates;
} StgTVar;
```

---

## TRecEntry

```c
typedef struct {
  StgTVar                   *tvar;
  StgClosure                *expected_value;
  StgClosure                *new_value;
  StgInt                     num_updates;
} TRecEntry;
```

---

## Invariant

```c
typedef struct {
  StgHeader      header;
  StgClosure    *code;
  StgTRecHeader *last_execution;
  StgWord        lock;
} StgAtomicInvariant;
```

---

## TRec

```c
typedef struct StgTRecHeader_ StgTRecHeader;
struct StgTRecHeader_ {
  StgHeader                  header;
  struct StgTRecHeader_     *enclosing_trec;
  StgTRecChunk              *current_chunk;
    // 這是 chunks of TRecEntry
  StgInvariantCheckQueue    *invariants_to_check;
    // 這是 list of StgAtomicInvariant
  TRecState                  state;
};
```

---

## transaction 的狀態

  * `TREC_ACTIVE`

    正在執行的

  * `TREC_CONDEMNED`

    發生問題的狀態.. 視為掉進平行宇宙, user (pragrammer) 可以假裝這種情況從來沒有發生過

  * `TREC_COMMITTED`

    確定要成功 commit, 正在寫入資料到現實的

  * `TREC_ABORTED`

    確定要放棄的, 正在清除自己以及放掉 lock

  * `TREC_WAITING`

    看了現實中的 `TVar` 情形以後驚呆了, 正在等待現況改變的

---

## 新增 transaction 的動作

  * 嗯, 沒什麼特別的.. 就是新增一個 TRec

    不過如果是巢狀的 transaction, 要記錄一下它的上層是誰

---

## 在 transaction 裡讀取一個 TVar

  * 找找看自己的 `TRec` 裡面有沒有這個 `TVar` 的記錄 (`TRecEntry`)

      - 有的話, 採用它的 `new_value`

      - 沒有的話, 遞迴找找自己祖先們有沒有

          + 有的話, 把它複製一份到自己的 TRec

            裡面沿用祖先記錄的 `expected_value` 與 `new_value`

          + 沒有的話, 查一下它在現實中的資料

            在自己 TRec 裡新增一個 `TRecEntry`, 其中

            `expected_value` 與 `new_value` 都設為它的 `current_value`

  * 將上述各種路徑所找到的 `TRecEntry` 中的 `new_value` 當作讀取的結果

---

## 在 transaction 裡寫入一個 TVar

  * 和前一頁讀取 `TVar` 的動作幾乎一樣,

    除了最後一步改成是把新值寫入 `TRecEntry` 的 `new_value`

    然後不用有什麼讀取的結果

---

## 在 transaction 裡加入一個 invariant

  * 用指定的檢查程式新增一個 invariant

  * 掛在 `TRec` 上

---

## abort 一個 transaction

  * 如果是 top-level transaction, 就把它清乾淨

      - 如果狀態是 `TREC_WAITING`, 要從各 `TVar` 的 watch queue 退出來

  * 如果是 nested transaction, 要把自己已看過的 TVar 原值 (`expected_value`) 加進還無此 `TVarEntry` 的上層

      - 不放 `new_value` 因為是 abort
      - abort 還要留存記錄, 是因為要確認這個 abort 是真的會發生, 不是發生在平行宇宙裡

---

## transaction 遇到 retry

  * 如果還有 `orElse` block 存在的話, 把目前這一個 block abort (nested abort),
    然後開一個新的 nested `TRec` 跑那一個 block

  * 如果已經沒有 `orElse` 了. 確認所有已讀過的 TVar 們後來都沒有被別人改過, 並且把它們 lock 住

      - lock 輕輕搶, 如果沒搶到 lock, 或發現值已被改過, 就送平行宇宙 (`TREC_CONDEMNED`)
          + 整個 transaction 爾後會就地重跑 (也許重跑以後不需要 retry)
      - 要把它們 lock 住, 因為接下來要把自己的 thread (`TSO`) 加到它們身上的 watch queue, 待它們的值改變的時候要喚醒自己

  * 狀態進入 `TREC_WAITING`

  * 回到 GHC 的 scheduler 把這個 thread 停住以後才從 scheduler unlock 這些 `TVar`, 以避免 unlock 之後與 thread 停住以前有任何 `TVar` 被改值,
    那會在 thread 停機以前還在跑的時候就先被喚起..

---

## 在 transaction 中遇到 exception

  * 如果是 synchronous exception

      - 檢查有沒有掉進平行宇宙, 如果是在平行宇宙的話, 不算.. 重跑

      - 否則 abort 這個 transaction, 然後把 exception 往外丟

  * 如果是 asynchronous exception

      - 執行間會啟用 mask 把 asynchronous exception 暫停掉, 所以不會發生

      - 如果是 `TREC_WAITING` 狀態的話, 就把它清掉

---

## commit 一個 nested transaction

  * 先 lock(輕輕) 並檢查所有要更動的 `TVar`, 不更動的 `TVar` 不用 lock, 但是記下它的 version number (`num_updates`)

      - 檢查 `expected_value` 有沒有變動

  * 再檢查沒有不更動的 `TVar` 的 version number 有沒有變

  * 然後開始寫入改變...(寫到上層, 不是寫到現實.. 因為它只是內層 transaction)

      - 一邊 unlock 這些被 lock 的 `TVar` 並且把對 entry 內容的改變寫進上層 `TRec` 的 `TRecEntry` (若缺項就新增)

(以上所有要檢查的事項如果任何一件沒通過, 就送平行宇宙候斬)

---

## commit 一個 transaction

  * commit 前, 找出需要檢查的 invariant 們

      - 在這個 transaction 裡出現的 `always`, `alwaysSucceeds` 所創造出來的新 invariant 會在執行時把自己掛到 `TRec` 上
      - 這次 transaction 執行過程中有改動到的 `TVar` 所屬的 watch queue 中出現的 invariant
          + 讀取 `TVar` watch queue 的過程會 lock(用力) `TVar`, 掃完一個 `TVar` 的 watch queue 就 unlock
          + 以前成功執行過的 invariant, 會把自己勾在最後一次執行的時候有存取過的 `TVar` 們的 watch queue 裡
          + 並且在自己的 `last_execution` 記錄那時執行時建立的 sub transaction, 用來記錄當時用到的 `TVar` list

---

## commit 一個 transaction

  * 然後檢查這些 invariant

      - 一一執行 (順序不重要, 其實心情好想同時執行也可以, 不過 GHC 選擇一一執行), 分別建立一個專屬的 sub transaction 來執行
      - 執行完無論成功與否都要把自己 abort 掉以去除任何更動 `TVar` 的影響, 然後記錄成功與否.
      - 如果有任何的失敗或 retry.. 當下就先檢查是否有掉進平行宇宙, 是的話就丟 exception, 或 retry(wait) 整個主 transaction

    PS. 回去查資料確認原本寫的這樣沒有錯

  * 一一重新 lock(輕輕) 並檢查剛剛碰過的 invariant 有沒有掉進平行宇宙

  * lock 並檢查所有要更動的 `TVar`, 不更動的 `TVar` 不用 lock, 但是記下它的 version number (`num_updates`)

      - 檢查 `expected_value` 有沒有變動

---

## commit 一個 transaction

  * 確定可以 commit 了

  * 一一 unlock invariant
      - 把它們的 `last_execution` 改記錄為這一次執行的 sub transaction 上
      - 分別把它們改勾這次執行時有用到的 `TVar` 的 watch queue 上 (之前勾的要放掉)

  * 寫入 `TVar` 的新資料到現實中, 並且遞增 `TVar` 的 `num_updates`

      - (如何避免 `num_updates` 破表的處理我就略過好了)
      - 一邊改動 `TVar` 一邊把這個 `TVar` watch queue 裡的 thread 們叫醒

---

## transaction 要 rewait

當一個 thread 被一個等待的改變的 `TVar` 喚起以前

  * 檢查最後一次執行的過程是否仍然沒有掉進平行宇宙

      - 是, 那只好繼續等
      - 否, 把自己 abort, 然後重跑

---

## 將這實作用在別的語言額外需注意的點一

  * 寫在 `atomically` block 裡的程式與 invariant 的 check 程式不可以存取 `TVar` 以外的東西:

      - 可以存取自己裡面的 local variable
      - 可以讀取 block 建立時給的 constant parameter
      - C 語言沒有 runtime 建立的 code block 這種東西 (...不要 shellcode ><)

        可以想像 C++ 的 functor 那種自帶 state 又像函數的東西,
        然後限制說 state 建立之後就不可以再改變

      - 也不可以呼叫使用任何有 side-effect 或是會往外偷看 (例如讀檔或讀取 mutable global variable 或 `time()`),
        或是會內部狀態改變的函數 (例如 `rand()`)

---

## 將這實作用在別的語言額外需注意的點一

  * 因為這些 code block 要允許被任意順序執行任意多次又可以 rollback, 而且當整個環境的 `TVar` 的值一樣的時候,
    同一個 code block 不管怎麼執行都要出一樣的結果

    然後在 invariant 的 check code 要記得也不可以讓它改到 `TVar`, 它們只能作 check

  * Haskell 的程式碼用 type 把 pure 程式與有 side-effect 的程式分開,
    只要是寫在 `STM a` 這種長相的 type 的 code block, 就只能存取 `TVar` 以及建構它時讀到的 constant

    (嗯, 因為 Haskell 的資料都是 immutable, 所以讀到的自動都是 constant)

    check code 的部分 GHC 實作是丟到 sub transaction 裡面執行, 然後一律 abort 來消除改到 `TVar` 的動作.

---

## 將這實作用在別的語言額外需注意的點二

  * 要安排放進 `TVar` 的資料結構, 要想好它鎖定的範圍到哪裡.

    規劃要由 STM 照顧的範圍內只能在 transaction 裡面的 read 函數整個讀取,
    而且只能經由 STM 的 write 函數整個更動.

    例如說想鎖整個資料, 但因為資料有點大坨, 希望避免 copy 所以放進 `TVar` 的只是一個指標,
    STM 只有保護到指標不被變動, 而沒有保護到指標所指向的那塊記憶體, 使用者就需要自我約束.

  * Haskell 是利用它所有的資料都是 immutable 的特性來劃出要鎖住的範圍,
    所以可以只鎖指標就鎖到整個.

    Rust 的 memory barrier 應該可以作到類似效果.

---

## 將這實作用在別的語言額外需注意的點三

  * 如果是採取 MVCC 實作, 考量安排複製資料的成本, 盡可能兼顧共享與不踩壞別的宇宙的資料

  * Haskell 利用資料都是 immutable 的特性, 所以指標共享是安全的

  * 如果不是採取 MVCC 實作, 小心 deadlock

---

## 將這實作用在別的語言額外需注意的點四

  * 如果你的語言有原生的 exception, 或是 process signal, 或是 C 語言 setjmp/longjmp 意味的東西, 要分辨 exception 的種類

      - 當從裡面要往外丟 exception 的時候,
        要記得出去以前要攔下來確認是不是平行宇宙裡的 exception,
        如果是來自平行宇宙, 要像是從來沒發生過地河蟹掉, 不能真的丟出去..

      - 如果是從天外飛進來的 asynchronous exception, 像是外部 kill 進來的 signal,
        不該是皇城裡面河蟹得掉的東西, 那就還是要中止自己

---

## 參考資料

  + [GHC STM Notes](http://fryguybob.github.io/STM-Commentary/) by Ryan Yates 2013.5.17

  * [Transactional memory with data invariants](http://research.microsoft.com/en-us/um/people/simonpj/papers/stm/stm-invariants.pdf)

  + [GHC 原始碼](https://github.com/ghc/ghc)

---

## Futher Read

  + [DSTM](http://hackage.haskell.org/package/DSTM) - Haskell Distributed STM
