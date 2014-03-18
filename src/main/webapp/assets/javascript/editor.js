function defaultLocalFileName() {
  return "epiceditor-" +  (Math.random() * 100000); 
}

function initEpicEditor(textareaID, epicFile) {
  
  var file = (epicFile == undefined || epicFile == "") ? defaultLocalFileName() : epicFile;

  var opts = {
    basePath: '/assets/epiceditor',
    textarea: textareaID,
    localStorageName: file,
    theme: {
      base: '/themes/base/epiceditor.css',
      preview: '/themes/preview/test.css',
      editor: '/themes/editor/epic-dark.css'
    },
    string: {
      togglePreview: '切換到預覽模式',
      toggleEdit: '切換到編輯模式',
      toggleFullscreen: '進入全螢幕'
    },
    button: {
      bar: true
    },
    autogrow: true,
    autogrow: {
      minHeight: 300,
      scroll: true
    }
  }
  
  return new EpicEditor(opts).load();
}

