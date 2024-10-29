// Made in Kick Assembler
.label spriteMemory = $3000

* = spriteMemory // spriteMemory is where you want your sprites

// Load the GIF with the sprite font, each letter in a 64x21 grid
.var spriteFont = LoadPicture("fontt1pil.gif",List().add($ffffff,$000000))
.var music = LoadSid("zoomakiaksi.sid")
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
* = spriteMemory + fontMap.get(l).charAt(p)*64 "Sprite" // determine memory location
    .print "perus: " + fontMap.get(l).charAt(p)
    .print "64: " + fontMap.get(l).charAt(p)*64
    .print "p: " + p

// Transfer the graphics in the GIF to the sprite
.fill 63, spriteFont.getSinglecolorByte((p*3)+mod(i,3), l*21+floor(i/3))

} // for-loop p
} // for-loop l
    .const SPRITESPACING   = 43    // minimal possible spacing
    .const SINLEAP         = 20    // choose anything here to change sine wave
    .const SCROLLSPEED     = 4     // lower value is slower
    .const SPRITEPOINTER   = $07f9
    .const PILLIPOINTER = $07f8
    .const PILLIMEMORY = spriteMemory + fontMap.get(1).charAt(12)*64 

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

    // Pillihomz
    lda #(>PILLIMEMORY<<2)       // Load the high byte of PILLIMEMORY
    sta PILLIPOINTER        // Store it at PILLIPOINTER
    lda #$04
    sta $d027
    lda #$10
    sta $d000
    lda #$00
    sta $d001

    // MUSIX
    lda #$00                  // Load 0 into accumulator
    tax                       // Transfer 0 to X register
    tay                       // Transfer 0 to Y register
    lda #music.startSong-1    // Load the startSong-1 address into accumulator
    jsr music.init            // Initialize music routine at $1B24


    ldx #$00
!:  lda #$a0               // init with spaces in all sprite pointers
    sta SPRITEPOINTER,x
    lda #$01               // color
    sta $d028,x            // set sprite colours
    inx
    cpx #$07
    bne !-

    lda #$01               // init IRQ
    sta $d01a
    lda #$7f
    sta $dc0d
    sta $dd0d
    lda $dc0d
    lda $dd0d

    lda #30
    sta $d012
    lda #$1b //00011011
    sta $d011

    ldx #<irq              // set pointers to FLD IRQ routine
    ldy #>irq
    stx $0314
    sty $0315

    cli

    jmp *

irq:
    asl $d019

    jsr music.play            // Play music

             //Fine-tune with cycle-exact instructions
    nop                     // Adjust timing to get precise alignment
    nop
    lda #$01
    sta $d017               // Enable sideborder opening with X-expansion
    lda scrollpos+1            // X-position of 1st sprite
    sec
    sbc #SCROLLSPEED           // decrease with SCROLLSPEED
    bpl notext                 // skip text fetch if sprite 2 can still move left

        ldx #$00               // shift content of sprites pointers
    scrollpointers:
        lda SPRITEPOINTER+1,x
        sta SPRITEPOINTER,x
        inx
        cpx #$06
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
        .break
        sta SPRITEPOINTER+6     // store new letter in sprite 8 pointer

        inc textpointer+1      // increase scroll text pointer
        bne !+
        inc textpointer+2
    !:
        lda #SPRITESPACING     // move sprite 2 to right most position
notext:
    sta scrollpos+1

    ldx #$00                   // position other sprites relative to sprite 1
scrollpos:
    lda #$18                   // set new X-coord for all sprites
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

    lda #$00                // Reset $d017 to close sideborder after usage
    sta $d017

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
    .fill 256, -50 + 15.5*sin(toRadians(i*(3*360)/256))

scrolltext:
    .text "spaggession is here! this is your invitation to the real party nearby in undisclosed location! be there or be lame! fuckings to lamers and the ones who don't pay their debts!"
    .byte $00

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