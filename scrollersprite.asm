// Made in Kick Assembler

.macro SetBorderColor(color) {
	lda #color
	sta $d020
}

.macro SetBackgroundColor(color) {
	lda #color
	sta $d021
}

.macro SetBgAndBorderColor(bgColor, borderColor) {
    SetBackgroundColor(bgColor)
    SetBorderColor(borderColor)
}

.macro SetMultiColor1(color) {
	lda #color
	sta $d022
}

.macro SetMultiColor2(color) {
	lda #color
	sta $d023
}

.macro SetMultiColor3(color) {
    lda #color
    sta $d024
}

.macro SetMultiColor4(color) {
    lda #color
    sta $d025
}

.macro SetMultiColor5(color) {
    lda #color
    sta $d026
}

.macro SetMultiColorMode() {
	lda	$d016
	ora	#16 // New: 00010110 Default: $C8, %11001000.
	sta	$d016
}

.label spriteMemory = $2000

* = spriteMemory // spriteMemory is where you want your sprites

.var bgColor = $00
.var firstColor = $04
.var secondColor = $02
.var thirdColor = $01
// Load the GIF with the sprite font, each letter in a 64x21 grid (24px x 42px is the actual size of each letter)
.var spriteFont = LoadPicture("fontgrip1234567.gif", List().add($000000, $cc44cc, $ffffff, $00cc55))

// Create a List() that contains the letters in your font
//  in the order as they appear in the GIF
.var fontMap = List()

// Define sprite width and height
.var spriteWidth = 24
.var spriteHeight = 21

// Parse the strings (var l = lines) in the fontMap List()
.eval fontMap.add("abcdefghijklmnopqrstuvwxyz,.!?:-")	// content of line (32 letters)
.eval fontMap.add(@"abcdefghijklmnopqrstuvwxyz,.!?:-")	// content of line (32 letters)
.print toIntString(spriteFont.width) + "x" + toIntString(spriteFont.height) + "px"	// print width and height of the font

.for (var l=0; l<fontMap.size(); l++){		// loop through lines
.for (var p=0; p<fontMap.get(l).size(); p++){	// loop through letters

    // sprite
    // a 0, b 1, c 2
    // a (0, 64), b (128, 192), c (256, 320)
    .var address = (p * 128) + (l * 64)	// determine memory location
    .print "p:" + toIntString(p) + " " + fontMap.get(l).charAt(p) + ", l:" + toIntString(l) + ", address:" + toIntString(address) + ", x: " + toIntString((p*3)+mod(1,3)) + ", y:" + toIntString(l*21+floor(1/3))
    * = spriteMemory + address

    .fill 63, spriteFont.getMulticolorByte((p*3)+mod(i,3), l*21+floor(i/3))
}
}
.print "Sprite memory: " + toHexString(spriteMemory)
    .const SPRITESPACING   = 43    // minimal possible spacing
    .const SINLEAP         = 20    // choose anything here to change sine wave
    .const SCROLLSPEED     = 4     // lower value is slower
    .const SPRITEPOINTER   = $07f8

*=$0801
    BasicUpstart($0810)

*=$0810
    sei
    jsr $e544              // KERNAL: clear screen

    SetBgAndBorderColor(bgColor, bgColor)

    lda #$00               // reset sine wave pointers
    sta sinreset+1
    sta sinwave+1

    ldx #<scrolltext       // reset scroll text pointer
    ldy #>scrolltext
    stx textpointer+1
    sty textpointer+2

    lda #$ff            // bits of sprite 0-7
    sta $d015           // turn on all sprites
    lda $d01c
    ora #%11111111     // Set all sprites multicolor
    sta $d01c
    SetMultiColorMode()
    SetMultiColor4(firstColor)
    SetMultiColor5(secondColor)

    ldx #$00
!:  lda #$a0               // init with spaces in all sprite pointers
    sta SPRITEPOINTER,x
    lda #thirdColor
    sta $d027,x            // set sprite colours
    inx
    cpx #$08
    bne !-

    lda #$01               // init IRQ
    sta $d01a
    lda #$7f
    sta $dc0d
    sta $dd0d
    lda $dc0d
    lda $dd0d

    lda #$32               // arbitrary value
    sta $d012
    lda #$1b
    sta $d011

    ldx #<irq              // set pointers to IRQ routine
    ldy #>irq
    stx $0314
    sty $0315

    cli

    jmp *

irq:
    inc $d019

    lda scrollpos+1            // X-position of 1st sprite
    sec
    sbc #SCROLLSPEED           // decrease with SCROLLSPEED
    bpl notext                 // skip text fetch if sprite 1 can still move left

        ldx #$00               // shift content of sprites pointers
    scrollpointers:
        lda SPRITEPOINTER+1,x
        sta SPRITEPOINTER,x
        inx
        cpx #$07
        bne scrollpointers
        lda sinreset+1         // shift sine wave
        clc
        adc SINLEAP            // this fixes sine offset
        adc #$03               // when resetting sprite position
        sta sinreset+1
    textpointer:
        lda scrolltext         // get next letter from scroll text
        bne noreset            // if not #$00 (end indicator)

        lda #<scrolltext       // reset scroll text pointer when letter is $00
        sta textpointer+1
        lda #>scrolltext
        sta textpointer+2
        jmp textpointer        // read new letter

    noreset:
        clc
        adc #(>spriteMemory<<2) // correct for location of sprite font
        sta SPRITEPOINTER+7     // store new letter in sprite 8 pointer

        inc textpointer+1      // increase scroll text pointer
        bne !+
        inc textpointer+2
    !:
        lda #SPRITESPACING     // move sprite 1 to right most position
notext:
    sta scrollpos+1

    ldx #$00                   // position other sprites relative to sprite 1
scrollpos:
    lda #$18                   // set new X-coord for all sprites
    sta $d000
    clc
    adc #SPRITESPACING
    sta $d002
    clc
    adc #SPRITESPACING
    sta $d004
    clc
    adc #SPRITESPACING
    sta $d006
    clc
    adc #SPRITESPACING
    sta $d008
    clc
    adc #SPRITESPACING
    sta $d00a
    bcc !+
    ldx #%11100000             // take care of MSB
    clc
!:  adc #SPRITESPACING
    sta $d00c
    bcc !+
    ldx #%11000000
    clc
!:  adc #SPRITESPACING
    sta $d00e
    bcc !+
    ldx #%10000000
!:  stx $d010                 // set proper sprite MSB

sinreset:
    ldx #$00                  // sine wave counter
    stx sinwave+1             // store in sine wave pointer
    inc sinreset+1
    ldy #$00
sinwave:
    lda sindata               // read sine wave data
    sta $d001,y               // store in Y-coords sprites
    lda sinwave+1
    clc
    adc #SINLEAP              // to make wave more interesting
    sta sinwave+1             // increase sine wave pointer by SINLEAP
    iny
    iny
    cpy #$10                  // next sprites
    bne sinwave
    jmp $ea31                 // end of IRQ1

.align $100
sindata:
    .fill 256, 120 + 15.5*sin(toRadians(i*(3*360)/256))

scrolltext:
    .text "a a a a a a a b b b b b b b cdefghijklmnopqrstuvwxyz,.!?:-"
    .byte $00
