// Made in Kick Assembler
.label screenRAM = $4000      // Move screen RAM here
.label colorRAM = $d800       // Move color RAM here
.label bitmapMem = $6000      // Bitmap 
.label spriteMemory = $5000   // Move sprites
.label MUSIC_LOAD = $1000     // New location where we want the music
.label MUSIC_INIT = MUSIC_LOAD  // Init routine at start of music
.label MUSIC_PLAY = MUSIC_LOAD + $03  // Play routine is at +$03


.const KOALA_TEMPLATE = "C64FILE, Bitmap=$0000, ScreenRam=$1f40, ColorRam=$2328, BackgroundColor = $2710" //4711 Bitmap,ScreenRam,ColorRam,BackgroundColor Files from koalapaint
.var picture = LoadBinary("spagfinalko2.kla", KOALA_TEMPLATE)

*=screenRAM "ScreenRAM";            .fill picture.getScreenRamSize(), picture.getScreenRam(i)
*=colorRAM "ColorRAM"; colorRam:  .fill picture.getColorRamSize(), picture.getColorRam(i)
*=bitmapMem "Bitmap";            .fill picture.getBitmapSize(), picture.getBitmap(i)


.macro LoopKoala() {
!loop:
.for (var i=0; i<4; i++) {
   lda colorRam+i*$100,x
   sta $d800+i*$100,x
}
inx
bne !loop-
}

* = spriteMemory "Sprites" // spriteMemory is where you want your sprites

// Load the GIF with the sprite font, each letter in a 64x21 grid
.var spriteFont = LoadPicture("fontt1pil.gif",List().add($ffffff,$000000))

.var music = LoadBinary("zoomakiaksi.sid")

*=MUSIC_LOAD "Music"
.fill music.getSize()-126, music.get(i+126)  // Skip the PSID header
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
* = spriteMemory + fontMap.get(l).charAt(p)*64 "Sprites" // determine memory location
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
    .const SPRITEPOINTER   = screenRAM+$03f9
    .const PILLIPOINTER = screenRAM+$03f8
    .const PILLIMEMORY = spriteMemory + fontMap.get(1).charAt(12)*64 

*=$0801
    BasicUpstart($0810)

*=$0810
    sei
    jsr $e544              // KERNAL: clear screen


    lda $dd00
    and #%11111100
    ora #%00000010              // Set to bank 1 ($4000-$7FFF) 
    sta $dd00                   // %10, 2: Bank #1, $4000-$7FFF, 16384-32767.

    // Set up screen and bitmap memory
    lda #%00001000    // Screen at $4000, bitmap at $6000
    sta $d018             //VIC bank adress+adress

    // Enable bitmap mode
    lda #$3b             // BMM=1, DEN=1, RSEL=1
    sta $d011
    lda #$d8             // MCM=1
    sta $d016

    // Set colors
    lda #$00
    sta $d020            // Border color
    lda #picture.getBackgroundColor()
    sta $d021            // Background color

    // Initialize color data
    ldx #$00
    LoopKoala()          // Copy initial color data


    lda #$00               // reset sine wave pointers
    sta sinreset+1
    sta sinwave+1

    ldx #<scrolltext       // reset scroll text pointer
    ldy #>scrolltext
    stx textpointer+1
    sty textpointer+2

    lda #$ff
    sta $d015            // Enable all sprites
    // Pilli sprite
    lda #(>PILLIMEMORY<<2)
    sta PILLIPOINTER
    lda #$04
    sta $d027
    lda #$20
    sta $d000
    lda #$00
    sta $d001

    // MUSIX init
    lda #$00
    tax
    tay
    lda #$01            // Start song 1
    jsr MUSIC_INIT


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
    lda #$3b //now: 00111011 default: 00011011
    sta $d011

    ldx #<irq              // set pointers to FLD IRQ routine
    ldy #>irq
    stx $0314
    sty $0315

    cli

    jmp *

irq:
    asl $d019

    jsr MUSIC_PLAY           // Play music
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
    .text "stoneman and lemming are spaggession! featuring rock of finnish gold! this is a new kind of concept we figured out after stoneman had done some dealings on boozembly 2024, and a bunch of people forgot to pay their debts to him! we figure this was because of alcohol and various substances, but if you feel the sting in your conscience, you have to contact stoneman immediately! this amazing intro is a production of damaged minds. we are currently boozing and smoking and whatnot on zoo 2024, and the party is amazing! it's lemming on the keys by the way. i'd like to greet everyone especially in nostalgia!! i'll be back some day. now i just had some of the coolest chats with my old groupnamtes der piipo from orange and pal from offence (and finnish gold, of course!). so i need to go find them so they can input their thoughts into this massive production. see you again in another scroller segment! "
    .text "Damones, soimme sydamia joimme juomia yhdessa kaljoissa pitkissa maksoissa, cheers by hoffi. spaggession is here! this is your invitation to the real party nearby in undisclosed location! be there or be lame! fuckings to lamers and the ones who don't pay their debts!"
    .byte $00

// Print the music info while assembling
.print ""
.print "SID Data"
.print "--------"
.print "location=$"+toHexString(MUSIC_LOAD)
.print "init=$"+toHexString(MUSIC_INIT)
.print "play=$"+toHexString(MUSIC_PLAY)
.print "size=$"+toHexString(music.getSize()-126)  // Size minus header
.print "First few bytes of music:"
.print "Byte 0: $" + toHexString(music.get(126))
.print "Byte 1: $" + toHexString(music.get(127))
.print "Byte 2: $" + toHexString(music.get(128))