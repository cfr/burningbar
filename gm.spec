 [
  { "_name": "User"
  , "id": "String"
  , "photo_id": "String"
  , "likes": "Int"
  , "name": "String"
  , "sex": "Int"
  , "status": "String"
  , "locale": "String"
  , "views": "String"
  , "date_reg": "String"
  , "photo_small": "String"
  , "photo": "String"
  , "age": "Int"
  , "city": "String"
  , "gifts": "[String]"
  , "is_liked": "Int"
  , "tag": "String"
  }
,
  { "_name": "Dialog"
  , "id": "String"
  , "imp": "Int"
  , "new": "Int"
  }
,
  { "_name": "GetDialogsResult"
  , "count": "Int"
  , "badge": "Int"
  , "items": "[Dialog]"
  , "push" : "GetDialogsPush"
  }
,
  { "_name": "GetDialogsPush"
  , "users": "{String:User}"
  }
,
  { "-pretty" : "example"
  , "-method" : "user.example"
  , "arg1" : "String"
  , "arg1" : "Int"
  , "-returns" : "Void"
  }
,
  { "-pretty" : "dialogs"
  , "-method" : "dialogs.getAllDialogs"
  , "-returns" : "GetDialogsResult"
  }
]
