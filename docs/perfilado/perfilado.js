document.querySelector('.form-button').addEventListener('click', function(event) {
    event.preventDefault(); // Prevent the default form submission

    var entity = document.getElementById('entity').value;
    var training_area = document.getElementById('training_area').value;
    var gender = document.getElementById('gender').value;
    var age = document.getElementById('option4').value;
    var epico_usage = document.getElementById('epico_usage').value;

    var formData = {
        entity: entity,
        training_area: training_area,
        gender: gender,
        age: age,
        epico_usage: epico_usage
    };

    console.log(JSON.stringify(formData));
});