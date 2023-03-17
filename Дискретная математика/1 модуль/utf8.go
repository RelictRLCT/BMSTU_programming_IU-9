package main

import "fmt"

func encode(utf32 []rune) []byte {
	var utf8 = make([]byte, 0)
	for i := 0; i < len(utf32); i++ {
		code_point := utf32[i]
		if code_point <= 0x7F {
			utf8 = append(utf8, byte(code_point))
		} else if code_point <= 0x7FF {
			utf8 = append(utf8, byte(0xC0|(code_point>>6)))
			utf8 = append(utf8, byte(0x80|(code_point&0x3F)))
		} else if code_point <= 0xFFFF {
			utf8 = append(utf8, byte(0xE0|(code_point>>12)))
			utf8 = append(utf8, byte(0x80|((code_point>>6)&0x3F)))
			utf8 = append(utf8, byte(0x80|(code_point&0x3F)))
		} else {
			utf8 = append(utf8, byte(0xF0|(code_point>>18)))
			utf8 = append(utf8, byte(0x80|((code_point>>12)&0x3F)))
			utf8 = append(utf8, byte(0x80|((code_point>>6)&0x3F)))
			utf8 = append(utf8, byte(0x80|(code_point&0x3F)))
		}
	}
	return utf8
}

func decode(utf8 []byte) []rune {
	var (
		utf32     = make([]rune, 0)
		i     int = 0
	)
	for i < len(utf8) {
		byte1 := rune(utf8[i])
		if byte1 < 0x80 {
			utf32 = append(utf32, rune(byte1))
			i += 1
		} else if byte1 < 0xE0 {
			byte2 := rune(utf8[i+1])
			utf32 = append(utf32, rune(((byte1&0x1F)<<6)|(byte2&0x3F)))
			i += 2
		} else if byte1 < 0xF0 {
			byte2 := rune(utf8[i+1])
			byte3 := rune(utf8[i+2])
			utf32 = append(utf32, rune(((byte1&0xF)<<12)|((byte2&0x3F)<<6)|(byte3&0x3F)))
			i += 3
		} else {
			byte2 := rune(utf8[i+1])
			byte3 := rune(utf8[i+2])
			byte4 := rune(utf8[i+3])
			utf32 = append(utf32, rune(((byte1&0x7)<<18)|((byte2&0x3F)<<12)|((byte3&0x3F)<<6)|(byte4&0x3F)))
			i += 4
		}
	}
	return utf32
}

func main() {
	ex := []rune{'π', 'μ', 'β', 'ο', '!'}
	res := encode(ex)
	result := decode(res)
	fmt.Println(ex)
	fmt.Println(res)
	fmt.Println(result)
	for _, x := range result {
		fmt.Printf("%s ", string(x))
	}
}
