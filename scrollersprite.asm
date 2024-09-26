// Made in Kick Assembler
.label spriteMemory = $2000

* = spriteMemory // spriteMemory is where you want your sprites

// Load the GIF with the sprite font, each letter in a 64x21 grid
.var spriteFont = LoadPicture("fontt1.gif",List().add($ffffff,$000000))

// Create a List() that contains the letters in your font
//  in the order as they appear in the GIF
.var fontMap = List()

// Add strings that contains all the letters for each line in the GIF
.eval fontMap.add("abcdefghijklmnopqrstuvwxyz")	// content of 1st line
.eval fontMap.add(@"0123456789\$27-+?!,.")	// content of 2nd line
                                                // (@ indicates escape code)

// Parse the strings (var l = lines) in the fontMap List()
.for (var l=0; l<fontMap.size(); l++){		// loop through lines

// Parse each string (var p = position)
.for (var p=0; p<fontMap.get(l).size(); p++){	// loop through letters

// The location in memory is determined by the value of the letter
* = spriteMemory + fontMap.get(l).charAt(p)*64 	// determine memory location

// Transfer the graphics in the GIF to the sprite
.fill 63, spriteFont.getSinglecolorByte((p*3)+mod(i,3), l*21+floor(i/3))

} // for-loop p
} // for-loop l
    .const SPRITESPACING   = 43    // minimal possible spacing
    .const SINLEAP         = 20    // choose anything here to change sine wave
    .const SCROLLSPEED     = 4     // lower value is slower
    .const SPRITEPOINTER   = $07f8

*=$0801
    BasicUpstart($0810)

*=$0810
    sei
    jsr $e544              // KERNAL: clear screen

    lda #$00               // Set border and background to black
    sta $d020              // Set the border color to black
    sta $d021              // Set the background color to black

    lda #$00               // reset sine wave pointers
    sta sinreset+1
    sta sinwave+1

    ldx #<scrolltext       // reset scroll text pointer
    ldy #>scrolltext
    stx textpointer+1
    sty textpointer+2

    lda #$ff
    sta $d015              // turn on all sprites

    ldx #$00
!:  lda #$a0               // init with spaces in all sprite pointers
    sta SPRITEPOINTER,x
    lda #$04               // purple
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
    inc $d019                  // Acknowledge the IRQ

    lda $d012                  // Check current raster line
    cmp #$30                   // Line where we want to disable the borders
    beq near_disable_borders    // Branch to a nearby label
    jmp skip_disable_borders    // If not equal, jump to skip the disabling

near_disable_borders:
    jmp disable_borders         // Perform an unconditional jump to the actual disable_borders code

skip_disable_borders:
    cmp #$f8                   // Line where we want to re-enable the borders
    beq near_enable_borders     // Branch to a nearby label
    jmp skip_enable_borders     // If not equal, jump to skip the enabling

near_enable_borders:
    jmp enable_borders          // Perform an unconditional jump to the actual enable_borders code

skip_enable_borders:

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
    ldx #%11100000             // Handle MSB for sprites
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
!:  stx $d010                 // Set proper sprite MSB

sinreset:
    ldx #$00                  // Reset sine wave counter
    stx sinwave+1             // Store in sine wave pointer
    inc sinreset+1
    ldy #$00
sinwave:
    lda sindata               // Read sine wave data
    sta $d001,y               // Store in Y-coords of sprites
    lda sinwave+1
    clc
    adc #SINLEAP              // Make wave more interesting
    sta sinwave+1             // Increase sine wave pointer by SINLEAP
    iny
    iny
    cpy #$10                  // Next sprites
    bne sinwave
    jmp $ea31                 // End of IRQ

disable_borders:
    lda $d011                  // Disable top/bottom borders
    and #%11110111             // Clear bit 4 of $d011
    sta $d011

    lda $d016                  // Disable side borders
    ora #%00001000             // Set bit 3 of $d016
    sta $d016
    jmp $ea31                  // Exit the interrupt

enable_borders:
    lda $d011                  // Re-enable top/bottom borders
    ora #%00001000             // Set bit 4 of $d011
    sta $d011

    lda $d016                  // Re-enable side borders
    and #%11110111             // Clear bit 3 of $d016
    sta $d016
    jmp $ea31                  // Exit the interrupt

.align $100
sindata:
    .fill 256, 120 + 15.5*sin(toRadians(i*(3*360)/256))

scrolltext:
    .text "spaggession is here! this is your invitation to the real party nearby in undisclosed location! be there or be lame! fuckings to lamers and the ones who don't pay their debts!"
    .byte $00
