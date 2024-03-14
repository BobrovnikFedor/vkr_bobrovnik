from telethon.sync import TelegramClient
from telethon.tl.functions.messages import GetHistoryRequest
from telethon.tl.types import PeerChannel
import csv
 
from telethon.tl.functions.messages import GetDialogsRequest
from telethon.tl.types import InputPeerEmpty
api_id = 27938819
api_hash = 'e484992715e529c0dd463cfd1eb8f1d7'
phone = '+79851079766'
 
client = TelegramClient(phone, api_id, api_hash,system_version="4.16.30-vxCUSTOM" )
client.start()
chats = []
last_date = None
size_chats = 1000
groups=[]
result = client(GetDialogsRequest(
            offset_date=last_date,
            offset_id=0,
            offset_peer=InputPeerEmpty(),
            limit=size_chats,
            hash = 0
        ))
chats.extend(result.chats)
for chat in chats:
    groups.append(chat)
print('Выберите номер группы из перечня:')
i=0
for g in groups:
   print(str(i) + '- ' + g.title)
   i+=1
g_index = input("Введите нужную цифру: ")
target_group=groups[int(g_index)]
all_messages = []
offset_id = 0
limit = 100
total_messages = 0
total_count_limit = 0
while True:
   history = client(GetHistoryRequest(
       peer=target_group,
       offset_id=offset_id,
       offset_date=None,
       add_offset=0,
       limit=limit,
       max_id=0,
       min_id=0,
       hash=0
   ))
   if not history.messages:
       break
   messages = history.messages
   for message in messages:
       all_messages.append(message.to_dict())
   offset_id = messages[len(messages) - 1].id
   if total_count_limit != 0 and total_messages >= total_count_limit:
       break
print("Сохраняем данные в файл...")  # Cообщение для пользователя о том, что начался парсинг сообщений.

with open("chats.csv", "w", encoding="UTF-8") as f:
    writer = csv.writer(f, delimiter=",", lineterminator="\n")
    writer.writerow(["message"])
    for message in all_messages:
        writer.writerow([message])
print("Парсинг сообщений группы успешно выполнен.")
