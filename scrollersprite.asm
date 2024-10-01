// Made in Kick Assembler
/*
Judges
Load Address
$1000
Init Address
$1B24
Play Address
$1B3A
*/
.macro SetBorderColor(color) {
	lda #color              // Load the border color into accumulator
	sta $d020               // Store the value in border color register $d020
}

.macro SetBackgroundColor(color) {
	lda #color              // Load the background color into accumulator
	sta $d021               // Store the value in background color register $d021
}

.macro SetBgAndBorderColor(bgColor, borderColor) {
    SetBackgroundColor(bgColor)  // Call macro to set background color
    SetBorderColor(borderColor)  // Call macro to set border color
}

.macro SetMultiColor1(color) {
	lda #color              // Load multicolor 1 color value into accumulator
	sta $d022               // Store in multicolor 1 register ($d022)
}

.macro SetMultiColor2(color) {
	lda #color              // Load multicolor 2 color value into accumulator
	sta $d023               // Store in multicolor 2 register ($d023)
}

.macro SetMultiColor3(color) {
    lda #color              // Load multicolor 3 color value into accumulator
    sta $d024               // Store in multicolor 3 register ($d024)
}

.macro SetMultiColor4(color) {
    lda #color              // Load multicolor 4 color value into accumulator
    sta $d025               // Store in multicolor 4 register ($d025)
}

.macro SetMultiColor5(color) {
    lda #color              // Load multicolor 5 color value into accumulator
    sta $d026               // Store in multicolor 5 register ($d026)
}

.macro SetMultiColorMode() {
	lda	$d016               // Load the control register $d016 into accumulator
	ora	#16                 // Enable multicolor mode by OR-ing with 16
	sta	$d016               // Store the modified value back into $d016
}

.var music = LoadSid("Crazy_Sample_II_intro.sid")   // Load the SID music file

.label spriteMemory = $2000    // Label for sprite memory, starts at $2000

* = spriteMemory              // Set the memory pointer to spriteMemory ($2000)

.var bgColor = $00            // Define a variable for background color
.var firstColor = $04         // Define a variable for the first sprite color
.var secondColor = $02        // Define a variable for the second sprite color
.var thirdColor = $01         // Define a variable for the third sprite color

// Load the GIF with the sprite font, each letter in a 64x21 grid
// Actual size of each letter is 24x42 pixels
.var spriteFont = LoadPicture("fontgrip1234567.gif", List().add($000000, $cc44cc, $ffffff, $00cc55))

// Create a List() that contains the letters in the font
//  in the order as they appear in the GIF
.var fontMap = List()

// Define sprite width and height
.var spriteWidth = 24         // Sprite width of 24 pixels
.var spriteHeight = 21        // Sprite height of 21 pixels

// Add character sets (lines) to fontMap
.eval fontMap.add("abcdefghijklmnopqrstuvwxyz,.!?:-")    // First line of characters
.eval fontMap.add(@"abcdefghijklmnopqrstuvwxyz,.!?:-")   // Second line of characters

// Print the width and height of the sprite font image
.print toIntString(spriteFont.width) + "x" + toIntString(spriteFont.height) + "px"

.for (var l=0; l<fontMap.size(); l++){    // Loop through each line of the fontMap
.for (var p=0; p<fontMap.get(l).size(); p++){  // Loop through each character in the line

    // sprite
    // a 0, b 1, c 2
    // a (0, 64), b (128, 192), c (256, 320)
    .var address = (p * 128) + (l * 64)    // Calculate the memory address for the sprite
    .print "p:" + toIntString(p) + " " + fontMap.get(l).charAt(p) + ", l:" + toIntString(l) + ", address:" + toIntString(address)
    * = spriteMemory + address             // Set the memory pointer to the sprite's address

    .fill 63, spriteFont.getMulticolorByte((p*3)+mod(i,3), l*21+floor(i/3))
    // Fill the memory with the sprite data from the font image
}
}
.print "Sprite memory: " + toHexString(spriteMemory)  // Print the sprite memory address

.const SPRITESPACING   = 43    // Set the minimal possible spacing between sprites
.const SINLEAP         = 20    // Define the leap for sine wave calculation
.const SCROLLSPEED     = 4     // Define the scroll speed (lower value = slower)
.const SPRITEPOINTER   = $07f8 // Pointer to the sprite data

*=$0801                      // Set the memory pointer to $0801 (BASIC start address)
    BasicUpstart($0810)       // Initialize BASIC upstart routine at $0810

*=$0810                      // Set the memory pointer to $0810 (start of code)
    sei                       // Disable interrupts
    jsr $e544                 // Jump to KERNAL clear screen routine

    SetBgAndBorderColor(bgColor, bgColor)   // Set the background and border colors

    lda #$00                  // Load 0 into accumulator (reset sine wave pointers)
    sta sinreset+1            // Store in sinreset pointer
    sta sinwave+1             // Store in sinwave pointer

    ldx #<scrolltext          // Load low byte of scroll text pointer into X
    ldy #>scrolltext          // Load high byte of scroll text pointer into Y
    stx textpointer+1         // Store low byte in textpointer+1
    sty textpointer+2         // Store high byte in textpointer+2

    lda #$ff                  // Load $ff (all bits on) into accumulator
    sta $d015                 // Enable all sprites (write to sprite enable register)
    lda $d01c                 // Load sprite multicolor enable register
    ora #%11111111            // Enable multicolor for all sprites
    sta $d01c                 // Store back in $d01c register
    SetMultiColorMode()       // Enable multicolor mode
    SetMultiColor4(firstColor)  // Set first sprite color
    SetMultiColor5(secondColor) // Set second sprite color
    lda #$00                  // Load 0 into accumulator
    tax                       // Transfer 0 to X register
    tay                       // Transfer 0 to Y register
    lda #music.startSong-1    // Load the startSong-1 address into accumulator
    jsr music.init            // Initialize music routine at $1B24

    ldx #$00                  // Load 0 into X register (for initialization)
!:  lda #$a0                  // Load $a0 (space character) into accumulator
    sta SPRITEPOINTER,x       // Store in sprite pointer memory
    lda #thirdColor           // Load third sprite color into accumulator
    sta $d027,x               // Store in sprite color register
    inx                       // Increment X register
    cpx #$08                  // Compare X with 8 (number of sprites)
    bne !-                    // If not equal, repeat the loop

    lda #$01                  // Load $01 (initialize IRQ) into accumulator
    sta $d01a                 // Store in interrupt control register
    lda #$7f                  // Load $7f into accumulator
    sta $dc0d                 // Enable CIA1 interrupt control register
    sta $dd0d                 // Enable CIA2 interrupt control register
    lda $dc0d                 // Read from CIA1 interrupt control register
    lda $dd0d                 // Read from CIA2 interrupt control register

    lda #$32                  // Load arbitrary value $32 into accumulator
    sta $d012                 // Store in raster compare register
    lda #$1b                  // Load $1b into accumulator
    sta $d011                 // Store in vertical scroll and display control register

    ldx #<irq                 // Load low byte of IRQ address into X register
    ldy #>irq                 // Load high byte of IRQ address into Y register
    stx $0314                 // Store low byte in IRQ vector
    sty $0315                 // Store high byte in IRQ vector

    cli                       // Enable interrupts

    jmp *                     // Jump to current address, infinite loop

irq:
    inc $d019                 // Acknowledge interrupt
    inc $d020                 // increment background color
    jsr music.play            // Play music
    dec $d020                 // decrement background color

    lda scrollpos+1           // Load X-position of the first sprite
    sec                       // Set carry flag (for subtraction)
    sbc #SCROLLSPEED          // Subtract scroll speed from X-position
    bpl notext                // If position is positive, skip text fetch

    ldx #$00              // Initialize X to 0 (start of sprite pointers shift)
    scrollpointers:
        lda SPRITEPOINTER+1,x // Load sprite pointer
        sta SPRITEPOINTER,x   // Store in previous sprite pointer
        inx                   // Increment X
        cpx #$07              // Compare X with 7 (number of sprites)
        bne scrollpointers    // Repeat if not equal

        lda sinreset+1        // Load sine wave reset value
        clc                   // Clear carry flag (for addition)
        adc SINLEAP           // Add sine leap to the value
        adc #$03              // Additional adjustment
        sta sinreset+1        // Store new value in sine wave reset

    textpointer:
        lda scrolltext        // Load next letter from scroll text
        bne noreset           // If not 0 (end indicator), skip reset

        lda #<scrolltext      // Reset scroll text pointer
        sta textpointer+1     // Store low byte
        lda #>scrolltext      // Load high byte
        sta textpointer+2     // Store high byte
        jmp textpointer       // Jump to textpointer for new letter

    noreset:
        clc
        adc #(>spriteMemory<<2) // Correct for location of sprite font
        sta SPRITEPOINTER+7     // Store new letter in sprite pointer

        inc textpointer+1     // Increment text pointer low byte
        bne !+                // If no carry, continue
        inc textpointer+2     // Otherwise, increment high byte
    !:
        lda #SPRITESPACING    // Load sprite spacing into accumulator
notext:
    sta scrollpos+1           // Store in X-position of first sprite

    ldx #$00                  // Load 0 into X register (for sprite positioning)
scrollpos:
    lda #$18                  // Load X-coordinate for first sprite
    sta $d000                 // Store in first sprite X-position register
    clc                       // Clear carry flag
    adc #SPRITESPACING        // Add sprite spacing
    sta $d002                 // Store in second sprite X-position register
    clc
    adc #SPRITESPACING        // Add sprite spacing
    sta $d004                 // Store in third sprite X-position register
    clc
    adc #SPRITESPACING        // Add sprite spacing
    sta $d006                 // Store in fourth sprite X-position register
    clc
    adc #SPRITESPACING        // Add sprite spacing
    sta $d008                 // Store in fifth sprite X-position register
    clc
    adc #SPRITESPACING        // Add sprite spacing
    sta $d00a                 // Store in sixth sprite X-position register
    bcc !+                    // Branch if carry clear
    ldx #%11100000            // Load most significant bits for sprite X-positions
    clc
!:  adc #SPRITESPACING        // Add sprite spacing
    sta $d00c                 // Store in seventh sprite X-position register
    bcc !+                    // Branch if carry clear
    ldx #%11000000            // Load most significant bits for sprite X-positions
    clc
!:  adc #SPRITESPACING        // Add sprite spacing
    sta $d00e                 // Store in eighth sprite X-position register
    bcc !+                    // Branch if carry clear
    ldx #%10000000            // Load most significant bits for sprite X-positions
!:  stx $d010                 // Store the MSB for all sprites

sinreset:
    ldx #$00                  // Initialize sine wave counter to 0
    stx sinwave+1             // Store in sine wave pointer
    inc sinreset+1            // Increment sine wave reset pointer
    ldy #$00                  // Initialize Y register to 0
sinwave:
    lda sindata               // Load sine wave data
    sta $d001,y               // Store in Y-coordinate of sprites
    lda sinwave+1             // Load current sine wave position
    clc                       // Clear carry flag
    adc #SINLEAP              // Add SINLEAP to make the wave more interesting
    sta sinwave+1             // Store new sine wave position
    iny                       // Increment Y register (move to next sprite)
    iny
    cpy #$10                  // Compare with 16 (number of sprites)
    bne sinwave               // Repeat if not all sprites are processed
    jmp $ea31                 // End of IRQ, jump back to main program

.align $100
sindata:
    .fill 256, 120 + 15.5*sin(toRadians(i*(3*360)/256))   // Fill with sine wave data

scrolltext:
    .text "a a a a a a a b b b b b b b cdefghijklmnopqrstuvwxyz,.!?:-"  // Define scroll text
    .byte $00                // End of text

*=music.location "Music"
.fill music.size, music.getData(i)
// Print the music info while assembling
.print ""
.print "SID Data"
.print "--------"
.print "location=$"+toHexString(music.location)
.print "init=$"+toHexString(music.init)
.print "play=$"+toHexString(music.play)
.print "songs="+music.songs
.print "startSong="+music.startSong
.print "size=$"+toHexString(music.size)
.print "name="+music.name
.print "author="+music.author
.print "copyright="+music.copyright

.print ""
.print "Additional tech data"
.print "--------------------"
.print "header="+music.header
.print "header version="+music.version
.print "flags="+toBinaryString(music.flags)
.print "speed="+toBinaryString(music.speed)
.print "startpage="+music.startpage
.print "pagelength="+music.pagelength
