$.fn.form.settings.rules["reallyNotEmpty"] = function(value) {
  return (value != undefined && $.trim(value) != "")
}

$.fn.form.settings.rules["isOnlyLetterOrDigit"] = function(value) {
  return (new RegExp('^[a-zA-Z0-9_]+$')).test(value)
}
