open Core_extended.Std

let () =
  Sendmail.send ~subject:"mail test" ~recipients:["till@janestreet.com"]
    "this is a test\nand here comes the ispo \n\
  lorem:\n\
Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nunc ut lobortis dui. \
Donec eget lacus vel ipsum dapibus molestie quis ut felis. Phasellus eu urna \
vel lectus tempor vestibulum. Sed in massa quis felis euismod accumsan dictum \
ut dui. Pellentesque aliquam aliquet blandit. Suspendisse et erat sed augue \
condimentum placerat quis ac tortor. Duis tincidunt blandit ultrices. Mauris \
et velit non leo scelerisque ornare nec eget sem. Pellentesque habitant morbi \
tristique senectus et netus et malesuada fames ac turpis egestas. Donec \
ullamcorper nunc eget nibh eleifend ac fringilla tellus tincidunt. Sed \
pulvinar pretium orci, vitae egestas neque tristique at. Cum sociis natoque \
penatibus et magnis dis parturient montes, nascetur ridiculus mus. Donec purus \
nisl, posuere sed placerat a, ultricies semper leo. Etiam tristique dignissim \
augue, a condimentum dolor porttitor quis. Phasellus vitae nisl purus, eget \
consectetur erat. Pellentesque adipiscing sagittis sapien nec consectetur. \
Vivamus vitae massa posuere est condimentum tincidunt. Nam in lacus mi. Nullam \
eget turpis leo."
