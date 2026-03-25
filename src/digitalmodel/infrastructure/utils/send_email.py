from exchangelib import Credentials, Account, FileAttachment, Message, Configuration, DELEGATE
from exchangelib.protocol import BaseProtocol, NoVerifyHTTPAdapter
import os.path
import urllib3

BaseProtocol.HTTP_ADAPTER_CLS = NoVerifyHTTPAdapter
urllib3.disable_warnings()
credentials = Credentials("sender_email@oxy.com", "password")
account = Account(
    primary_smtp_address="sender_email@oxy.com",
    credentials=credentials,
    config=Configuration(server="smtp.office365.com", credentials=credentials),
    autodiscover=False,
    access_type=DELEGATE,
)
m = Message(
    account=account,
    subject="This is the email subject",
    body="This is the body of the email message.",
    to_recipients=["alexander_lach@oxy.com"],
    cc_recipients=["other_person@oxy.com"],
)

with open("test_attachment.txt", "rb") as f:
    content = f.read()
m.attach(FileAttachment(name="attachment_name.txt", content=content))
m.send()
