## Semigroup

### What is Associativity?
**Associative** means that when you apply an operation multiple times, the grouping doesn't matter:
- `(a ⊕ b) ⊕ c = a ⊕ (b ⊕ c)`
- The parentheses can be moved around without changing the result

### What is a Semigroup?
A **semigroup** is a mathematical structure consisting of:
1. A set of elements
2. An associative binary operation

### Haskell's Semigroup Type Class---

## Giải thích tiếng Việt

### Tính chất kết hợp (Associativity) là gì?
**Kết hợp** có nghĩa là khi bạn áp dụng một phép toán nhiều lần, cách nhóm không quan trọng:
- `(a ⊕ b) ⊕ c = a ⊕ (b ⊕ c)`
- Có thể di chuyển dấu ngoặc mà không làm thay đổi kết quả

### Semigroup là gì?
**Semigroup** là một cấu trúc toán học gồm:
1. Một tập hợp các phần tử
2. Một phép toán nhị phân có tính kết hợp

### Ví dụ về tính kết hợp:

#### ✅ Phép toán có tính kết hợp (Semigroup):
- **Phép cộng**: `(1 + 2) + 3 = 1 + (2 + 3) = 6`
- **Phép nhân**: `(2 × 3) × 4 = 2 × (3 × 4) = 24`
- **Nối chuỗi**: `("Hello" + " ") + "World" = "Hello" + (" " + "World") = "Hello World"`
- **Nối danh sách**: `([1,2] ++ [3]) ++ [4] = [1,2] ++ ([3] ++ [4]) = [1,2,3,4]`

#### ❌ Phép toán KHÔNG có tính kết hợp (Không phải Semigroup):
- **Phép trừ**: `(10 - 5) - 2 = 3`, nhưng `10 - (5 - 2) = 7`
- **Phép chia**: `(24 ÷ 4) ÷ 2 = 3`, nhưng `24 ÷ (4 ÷ 2) = 12`
- **Phép lũy thừa**: `(2³)² = 64`, nhưng `2^(3²) = 512`

### Type Class Semigroup trong Haskell

```haskell
class Semigroup a where
  (<>) :: a -> a -> a  -- Phép toán kết hợp
```

### Ví dụ thực tế:

#### 1. Giỏ hàng (Shopping Cart)
```haskell
data ShoppingCart = ShoppingCart
  { items :: [String]
  , totalPrice :: Sum Double
  , itemCount :: Sum Int
  }

instance Semigroup ShoppingCart where
  cart1 <> cart2 = ShoppingCart 
    { items = items cart1 <> items cart2
    , totalPrice = totalPrice cart1 <> totalPrice cart2
    , itemCount = itemCount cart1 <> itemCount cart2
    }
```

#### 2. Validation (Thu thập lỗi)
```haskell
data Validation e a = Failure e | Success a

instance Semigroup e => Semigroup (Validation e a) where
  Failure e1 <> Failure e2 = Failure (e1 <> e2)  -- Kết hợp lỗi
  Failure e1 <> Success _ = Failure e1
  Success _ <> Failure e2 = Failure e2
  Success a1 <> Success _ = Success a1
```

### Lợi ích của Semigroup

**English**:
- **Parallelization**: Can split work across multiple threads and combine results
- **Incremental processing**: Can add new elements one by one
- **Flexibility**: Can change grouping strategy without affecting correctness
- **Abstraction**: Same interface for many different combining operations

**Tiếng Việt**:
- **Xử lý song song**: Có thể chia công việc ra nhiều luồng và kết hợp kết quả
- **Xử lý tăng dần**: Có thể thêm từng phần tử một cách tuần tự
- **Linh hoạt**: Có thể thay đổi cách nhóm mà không ảnh hưởng đến tính đúng đắn
- **Trừu tượng hóa**: Cùng một giao diện cho nhiều phép toán kết hợp khác nhau

### Ví dụ thực tế về song song hóa:

```haskell
-- Có thể tính song song
result1 = (a <> b) <> (c <> d)  -- Thread 1: a <> b, Thread 2: c <> d
result2 = ((a <> b) <> c) <> d  -- Tuần tự từ trái sang phải
-- result1 == result2 (nhờ tính kết hợp)
```

### Sự khác biệt với Monoid

**Semigroup**: Chỉ cần phép toán kết hợp `<>`
**Monoid**: Cần phép toán kết hợp `<>` + phần tử đơn vị `mempty`

```haskell
class Semigroup a => Monoid a where
  mempty :: a  -- Phần tử đơn vị
```

### Tại sao quan trọng?

**English**: Semigroup provides a foundation for many functional programming patterns. It allows us to abstract over any "combining" operation, making code more reusable and composable.

**Tiếng Việt**: Semigroup cung cấp nền tảng cho nhiều mẫu thiết kế lập trình hàm. Nó cho phép chúng ta trừu tượng hóa bất kỳ phép toán "kết hợp" nào, làm cho code có thể tái sử dụng và kết hợp được.