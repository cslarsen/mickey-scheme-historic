(import (mickey dynamic-library))

(define sdl (dlopen "src/libmickey-sdl.so" 'lazy))
(define initialize (dlsym sdl "initialize"))
(define set-video-mode (dlsym sdl "set_video_mode"))

(import (scheme write)
        (scheme base))

(display "Running init\n")
(initialize)

(display "Opening window\n")
(set-video-mode 640 480 'hwsurface 'fullscreen)

(display "Done\n")
