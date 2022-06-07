from Crypto.Cipher import AES

data = b'safak'

key = b'Sixteen byte key'
cipher = AES.new(key, AES.MODE_EAX)

nonce = cipher.nonce
ciphertext, tag = cipher.encrypt_and_digest(data)

print(ciphertext)