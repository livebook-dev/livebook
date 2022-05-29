const FileInput = {
    mounted() {
        this.el.onchange = async (event) => {
            let files = Array.from(event.target.files).map((file) => {
                let reader = new FileReader();

                return new Promise(resolve => {
                    reader.onload = () => {
                        resolve(reader.result)
                    };
                    reader.readAsBinaryString(file);
                });
            });

            let file_contents = await Promise.all(files);

            this.pushEventTo(this.el, 'change', { value: file_contents })
        }
    }
}

export default FileInput;
