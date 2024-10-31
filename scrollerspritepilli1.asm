// Made in Kick Assembler
.label spriteMemory = $3000
* = spriteMemory // spriteMemory is where you want your sprites

// Load the GIF with the sprite font, each letter in a 64x21 grid
.var spriteFont = LoadPicture("fontt1pil.gif",List().add($ffffff,$000000))
.var music = LoadSid("zoomakiaksi.sid")

.var line1 = "abcdefghijklmnopqrstuvwxyz"
.var line2 = @"0123456789\$27-+?!,."

// Calculate base addresses for each line
.var line1_base = spriteMemory          // First line starts at $3000
.var line2_base = spriteMemory + $c00   // Second line starts at $3c00

// Parse line 1 (lowercase letters)
.for (var p=0; p<line1.size(); p++) {
    * = line1_base + (p*64) "Sprite"
    .print "p: " + p 
    .print "Mem: " + toHexString(*) 
    .print "char: " + line1.charAt(p)
    .fill 63, spriteFont.getSinglecolorByte((p*3)+mod(p,3), 0+floor(p/3))
}

// Parse line 2 (numbers and symbols)
.for (var p=0; p<line2.size(); p++) {
    * = line2_base + (p*64) "Sprite"
    .print "p: " + p 
    .print "Mem: " + toHexString(*) 
    .print "char: " + line2.charAt(p)
    .fill 63, spriteFont.getSinglecolorByte((p*3)+mod(p,3), 1+floor(p/3))
}
    .const SPRITESPACING   = 43    // minimal possible spacing
    .const SINLEAP         = 20    // choose anything here to change sine wave
    .const SCROLLSPEED     = 2     // lower value is slower
    .const SPRITEPOINTER   = $07fa
    .const PILLIPOINTER = $07f9
    .const PILLIMEMORY = $3000



/// Create lookup table as a List
.var lookupList = List()

// Space and early special chars ($20-$2F)
.eval lookupList.add($40)        // $20 Space
.eval lookupList.add($e1)        // $21 ! 
.eval lookupList.add($40)        // $22 "
.eval lookupList.add($40)        // $23 #
.eval lookupList.add($40)        // $24 $
.eval lookupList.add($40)        // $25 %
.eval lookupList.add($40)        // $26 &
.eval lookupList.add($40)        // $27 ' 
.eval lookupList.add($40)        // $28 (
.eval lookupList.add($40)        // $29 )
.eval lookupList.add($40)        // $2a *
.eval lookupList.add($eb)        // $2b + 
.eval lookupList.add($ec)        // $2c , 
.eval lookupList.add($ed)        // $2d - 
.eval lookupList.add($ee)        // $2e .
.eval lookupList.add($40)        // $2f /

// Numbers ($30-$39)
.for(var i=0; i<10; i++) {
    .eval lookupList.add($f0+i)  // 0-9 unchanged
}

// Special chars ($3A-$3F)
.eval lookupList.add($40)        // $3a :
.eval lookupList.add($40)        // $3b ;
.eval lookupList.add($40)        // $3c <
.eval lookupList.add($40)        // $3d =
.eval lookupList.add($40)        // $3e >
.eval lookupList.add($ff)        // $3f ? - corrected to $ff

// @ and uppercase A-Z ($40-$5A)
.eval lookupList.add($40)        // @
.for(var i=0; i<26; i++) {
    .eval lookupList.add($40)    // A-Z
}

// More special chars ($5B-$60)
.for(var i=0; i<6; i++) {
    .eval lookupList.add($40)
}

// Lowercase a-z ($61-$7A)
.for(var i=0; i<26; i++) {
    .eval lookupList.add($c1+i)  // a-z unchanged
}


// Convert List to actual lookup table
sprite_lookup:
.for(var i=0; i<lookupList.size(); i++) {
    .byte lookupList.get(i)
}
.print ""
.print "Memory layout for line 1 (letters):"
.for (var p=0; p<line1.size(); p++) {
    .var char = line1.charAt(p)
    .var mem = spriteMemory + (p*64)
    .var ptr = mem/64
    .print "Character: '" + char + "' ASCII: $" + toHexString(char) + " at memory: $" + toHexString(mem) + " -> sprite pointer: $" + toHexString(ptr)
}

.print ""
.print "Memory layout for line 2 (numbers & symbols):"
.for (var p=0; p<line2.size(); p++) {
    .var char = line2.charAt(p)
    .var mem = spriteMemory + $c00 + (p*64)
    .var ptr = mem/64
    .print "Character: '" + char + "' ASCII: $" + toHexString(char) + " at memory: $" + toHexString(mem) + " -> sprite pointer: $" + toHexString(ptr)
}

.label textpointer_lo = $fb    // Zero page locations for text pointer
.label textpointer_hi = $fc
scrollpos: .byte 0             // Current scroll position
sinpos:    .byte 0             // Current sine position

    // for precalculating 
    .const BASE_Y = -50
    .const AMPLITUDE = 15.5
    .const SINE_STEPS = 256

    .var rasterLinesList = List()
    .for(var i=0; i<SINE_STEPS; i++) {
        .eval rasterLinesList.add(BASE_Y + AMPLITUDE*sin(toRadians(i*(3*360)/256)))
    }

*=$0801
    BasicUpstart($0810)

*=$0810
    sei
    jsr $e544              // KERNAL: clear screen

    lda #$00               // Set border and background to black
    sta $d020              
    sta $d021              

    lda #<scrolltext         // Initialize text pointer
    sta textpointer_lo
    lda #>scrolltext
    sta textpointer_hi

    ldx #$00
!:  lda #$a0                    // Spaceeee
    sta SPRITEPOINTER,x         // Initialize sprites 2-5 ($07fa-$07fd)
    lda #$01                    // White color
    sta $d029,x                 // Colors for sprites 2-5
    inx
    cpx #$04                    // Four sprites
    bne !-

    // MUSIX init
    lda #$00              
    tax                    
    tay                    
    lda #music.startSong-1 
    jsr music.init        

    // Pillihomz (sprite 1)
    lda #(>PILLIMEMORY<<2)       
    sta PILLIPOINTER            // $07f9 (sprite 1)
    lda #$04                    // Purple color
    sta $d028                   // Sprite 1 color
    lda #$10
    sta $d002                   // X position for sprite 1
    lda #$50
    sta $d003                   // Y position for sprite 1        

    lda #$fe                // Enable all the rest expect 0 sprite
    sta $d015    

    lda #$01               // init IRQ
    sta $d01a
    lda #$7f               // Disable CIA interrupts
    sta $dc0d
    sta $dd0d
    lda $dc0d
    lda $dd0d

    lda #$35               // Bank out KERNAL and BASIC
    sta $01                // Only if you don't need KERNAL functions anymore
    
    lda #50                // Set initial raster line
    sta $d012
    lda #$1b              // High bit clear
    sta $d011

    ldx #<irq1            // Set up first IRQ vector
    ldy #>irq1
    stx $fffe             // Use hardware vectors instead of KERNAL
    sty $ffff

    cli
    jmp *

.align $100
rasterlines:
    .for(var i=0; i<SINE_STEPS; i++) {
        .byte rasterLinesList.get(i)
    }

irq1:   
        sta saveA+1           
        stx saveX+1
        sty saveY+1

        lda $d012
!:      cmp $d012
        beq !-
!:      lda $d012
        ldx sinreset+1        
        lda $20     
        cmp $d012
        bne !-

        ldx #$08              
!:      dex
        bne !-
        bit $ea
        nop
        nop

        dec $d016             
        nop
        nop
        nop
        inc $d016             

        jsr handle_scroll     // First handle scroll
        jsr update_sine      // Then update positions
        jsr music.play       // Then music

        asl $d019            

saveA:  lda #$00             
saveX:  ldx #$00
saveY:  ldy #$00
        rti

handle_scroll:
    lda scrollpos       
    sec
    sbc #SCROLLSPEED      
    bpl no_update         

    // Move sprite pointers left $07fa
    lda $07fb              
    sta $07fa              
    lda $07fc              
    sta $07fb              
    lda $07fd              
    sta $07fc              

    // Get new character and handle text wrapping
    ldy #0
    lda (textpointer_lo),y   
    bne !+
    // Reset text if at end
    lda #<scrolltext
    sta textpointer_lo
    lda #>scrolltext
    sta textpointer_hi
    lda (textpointer_lo),y
!:
    lda (textpointer_lo),y
    sec
    sbc #$20                 // Convert ASCII to lookup table index
    tax
    lda sprite_lookup,x      // Get sprite pointer from table
    sta $07fd               // Store in last sprite position

    // Increment text pointer
    inc textpointer_lo
    bne !+
    inc textpointer_hi
!:
    lda #SPRITESPACING     
no_update:
    sta scrollpos

    // Position sprites and handle MSB
    clc
    adc #$30                // Start position
    sta $d004               // Sprite 2 X
    clc
    adc #SPRITESPACING
    sta $d006               // Sprite 3 X
    clc
    adc #SPRITESPACING
    sta $d008               // Sprite 4 X
    clc
    adc #SPRITESPACING
    sta $d00a               // Sprite 5 X
    
    // MSB handling
    lda #0
    adc #0                  // Get carry from last addition
    asl                     // Shift to correct position for sprites 2-5
    asl
    sta $d010               // Set MSB

    rts

update_sine:
    ldx sinpos
    lda sindata,x          // Get Y position from sine table
    sta $d003              // Sprite 1 (Pilli)
    sta $d005              // Sprite 2
    
    txa
    clc
    adc #SINLEAP
    tax
    lda sindata,x
    sta $d007              // Sprite 3
    
    txa
    clc
    adc #SINLEAP
    tax
    lda sindata,x
    sta $d009              // Sprite 4
    
    txa
    clc
    adc #SINLEAP
    tax
    lda sindata,x
    sta $d00b              // Sprite 5

    inc sinpos             // Update sine offset for next frame
    rts
sinreset:
    ldx #$00              
    stx sinwave+1         
    inc sinreset+1        
    ldx sinreset+1        
    
    lda sindata,x
    sta $d003            

    txa                  
    clc
    adc #SINLEAP         
    tax
    lda sindata,x        
    sta $d005            

    txa
    clc
    adc #SINLEAP         
    tax
    lda sindata,x
    sta $d007            

    txa
    clc
    adc #SINLEAP         
    tax
    lda sindata,x
    sta $d009            

    txa
    clc
    adc #SINLEAP         
    tax
    lda sindata,x
    sta $d00b            

    rts
sinwave:
    lda sindata           
    sta $d001,y          // Start from sprite 1 Y position
    lda sinwave+1
    clc
    adc #SINLEAP         
    sta sinwave+1        
    iny
    iny
    cpy #$10             // Up through sprite 7
    bne sinwave
    rts

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
