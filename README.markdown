# retrospect

## Requirements

- Java (JDK 5+)
- git (Windows: [msysgit](http://code.google.com/p/msysgit/)

## Installation (Unix)

- Download [Leiningen](https://raw.github.com/technomancy/leiningen/stable/bin/lein) (save as "lein")
- Make "lein" executable: `chmod +x lein`
- Download retrospect source: `git clone git@bitbucket.org:joshuaeckroth/retrospect.git`
- Go into the `retrospect` directory: `cd retrospect`
- Download the [data package](https://bitbucket.org/joshuaeckroth/retrospect/downloads/retrospect-data.zip) and unzip (creating the `data` folder)
- Start the retrospect player (as a test): `lein run -m retrospect.core --database "http://fier.ath.cx:5984/retrospect"`

## Installation (Windows)

- Download [Leiningen](https://github.com/downloads/technomancy/leiningen/leiningen-1.5.2-win.zip) (open the zip file, extract somewhere)
- Run `lein self-install` in a command prompt (in the newly extracted folder)
- Set up git with your SSH keys
- Download retrospect source (in a command prompt): `git clone git@bitbucket.org:joshuaeckroth/retrospect.git`
- Go into the `retrospect` directory: `cd retrospect`
- Download the [data package](https://bitbucket.org/joshuaeckroth/retrospect/downloads/retrospect-data.zip) and unzip (creating the `data` folder)
- Start the retrospect player (as a test): `lein run -m retrospect.core --database "http://fier.ath.cx:5984/retrospect"`
