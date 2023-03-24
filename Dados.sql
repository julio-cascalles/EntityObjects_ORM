CREATE TABLE Pessoa(
	id INTEGER identity(1,1) PRIMARY KEY,
	nome VARCHAR(30)
	/* [Medico,Paciente] */
);

CREATE TABLE Agenda(
	dia DATE,
	hora CHAR(5),
	medico INTEGER foreign key REFERENCES Medico(id),
	paciente INTEGER foreign key REFERENCES Paciente(id)
);