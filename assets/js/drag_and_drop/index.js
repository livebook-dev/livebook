const DragAndDrop = {
  mounted() {
    const dropZone = this.el.querySelector("[data-dropzone]");

    ["dragenter", "dragover"].forEach((eventName) => {
      dropZone.addEventListener(eventName, highlight, false);
    });

    ["dragleave", "drop"].forEach((eventName) => {
      dropZone.addEventListener(eventName, unhighlight, false);
    });

    function highlight(e) {
      dropZone.classList.add("bg-red-200");
      dropZone.classList.add("border-red-400");
    }

    function unhighlight(e) {
      dropZone.classList.remove("bg-red-200");
      dropZone.classList.remove("border-red-400");
    }
  },
};

export default DragAndDrop;
