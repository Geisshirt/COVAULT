## COVAULT - A passord manager written in COBOL.
⚠️ Disclaimer: Do not actually use - the encryption is (for now) very weak.

### How it works
The users main passwords is encrypted with it self as the hash and stored in a ´.csv´ file (in the future a proper database should be used). Any added passwords are stored as a pair of an identifier, e.g., a username, URL, etc. and the password it self. The identifier and password are both encrypted with the main password as the key. When adding a password one can either come up with one or chose to have one generated. Viewing the passwords you can either view a specific by searching after the identifier or view all stored passwords. The identifier and passwords are decrypted with the main password as the key and displayed.
