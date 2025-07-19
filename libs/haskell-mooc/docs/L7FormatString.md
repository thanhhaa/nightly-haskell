## Format string usign printf

### What is printf?
`printf` is a formatted printing function imported from `Text.Printf`. It works similarly to printf in C/C++ and allows you to format strings with placeholders.

### Breaking Down `printf "%02d" cents`

#### The Format String: `"%02d"`
- `%` - Starts a format specifier
- `0` - **Padding character**: Use `0` instead of spaces for padding
- `2` - **Minimum width**: The result will be at least 2 characters wide
- `d` - **Format type**: Decimal integer

#### Examples:
```haskell
printf "%02d" 5    -- Result: "05"
printf "%02d" 15   -- Result: "15"
printf "%02d" 123  -- Result: "123" (no truncation, just uses more space)
printf "%02d" 0    -- Result: "00"
```

### Common Format Specifiers---

## Giải thích tiếng Việt

### printf là gì?
`printf` là hàm in có định dạng được import từ `Text.Printf`. Nó hoạt động tương tự như printf trong C/C++ và cho phép bạn định dạng chuỗi với các placeholder.

### Phân tích `printf "%02d" cents`

#### Chuỗi định dạng: `"%02d"`
- `%` - Bắt đầu một format specifier (bộ chỉ định định dạng)
- `0` - **Ký tự padding**: Sử dụng `0` thay vì dấu cách để đệm
- `2` - **Độ rộng tối thiểu**: Kết quả sẽ có ít nhất 2 ký tự
- `d` - **Kiểu định dạng**: Số nguyên thập phân

#### Ví dụ:
```haskell
printf "%02d" 5    -- Kết quả: "05"
printf "%02d" 15   -- Kết quả: "15"  
printf "%02d" 123  -- Kết quả: "123" (không cắt bớt, chỉ sử dụng nhiều chỗ hơn)
printf "%02d" 0    -- Kết quả: "00"
```

### Tại sao sử dụng `%02d` cho tiền xu?

**English**: When displaying money, cents should always show two digits:
- 5 cents → "05" (not "5")
- 50 cents → "50" 
- This ensures consistent formatting like "$12.05" instead of "$12.5"

**Tiếng Việt**: Khi hiển thị tiền, xu phải luôn hiển thị hai chữ số:
- 5 xu → "05" (không phải "5")
- 50 xu → "50"
- Điều này đảm bảo định dạng nhất quán như "$12.05" thay vì "$12.5"

### Các Format Specifier phổ biến

| Format | Mô tả | Ví dụ |
|--------|-------|-------|
| `%d` | Số nguyên | `printf "%d" 42` → "42" |
| `%02d` | Số nguyên, đệm 0, tối thiểu 2 ký tự | `printf "%02d" 5` → "05" |
| `%03d` | Số nguyên, đệm 0, tối thiểu 3 ký tự | `printf "%03d" 7` → "007" |
| `%.2f` | Số thực, 2 chữ số thập phân | `printf "%.2f" 3.14159` → "3.14" |
| `%s` | Chuỗi | `printf "%s" "Hello"` → "Hello" |

### Ứng dụng thực tế

#### Trong Money type:
```haskell
renderMoney :: Money -> String
renderMoney (Money cents) = 
  let dollars = cents `div` 100
      remainingCents = cents `mod` 100
  in printf "$%d.%02d" dollars remainingCents
```

**Giải thích**:
- `printf "$%d.%02d"` tạo template "$số.sốsố"
- `%d` cho phần đô-la (không cần đệm)
- `%02d` cho phần xu (luôn 2 chữ số)

#### Ví dụ kết quả:
- `Money 1250` → "$12.50"
- `Money 507` → "$5.07"  
- `Money 5` → "$0.05"
- `Money 10000` → "$100.00"

### Lợi ích của printf

**English**:
- **Consistent formatting**: Always shows proper number of digits
- **Readable code**: Format string clearly shows the output pattern
- **Flexible**: Can combine multiple format specifiers
- **Familiar**: Similar to printf in many other languages

**Tiếng Việt**:
- **Định dạng nhất quán**: Luôn hiển thị đúng số chữ số
- **Code dễ đọc**: Chuỗi định dạng rõ ràng cho thấy mẫu đầu ra
- **Linh hoạt**: Có thể kết hợp nhiều format specifier
- **Quen thuộc**: Tương tự printf trong nhiều ngôn ngữ khác