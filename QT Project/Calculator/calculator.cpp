#include "calculator.h"
#include "./ui_calculator.h"

Calculator::Calculator(QWidget *parent):
    QMainWindow(parent),
    ui(new Ui::Calculator)
{
    ui->setupUi(this);
    ui->Screen->setText("0");
    connect(ui->Button0,SIGNAL(released()),this,SLOT(NumberPressed()));
    connect(ui->Button1,SIGNAL(released()),this,SLOT(NumberPressed()));
    connect(ui->Button2,SIGNAL(released()),this,SLOT(NumberPressed()));
    connect(ui->Button3,SIGNAL(released()),this,SLOT(NumberPressed()));
    connect(ui->Button4,SIGNAL(released()),this,SLOT(NumberPressed()));
    connect(ui->Button5,SIGNAL(released()),this,SLOT(NumberPressed()));
    connect(ui->Button6,SIGNAL(released()),this,SLOT(NumberPressed()));
    connect(ui->Button7,SIGNAL(released()),this,SLOT(NumberPressed()));
    connect(ui->Button8,SIGNAL(released()),this,SLOT(NumberPressed()));
    connect(ui->Button9,SIGNAL(released()),this,SLOT(NumberPressed()));
    connect(ui->ButtonA,SIGNAL(released()),this,SLOT(NumberPressed()));
    connect(ui->ButtonB,SIGNAL(released()),this,SLOT(NumberPressed()));
    connect(ui->ButtonC,SIGNAL(released()),this,SLOT(NumberPressed()));
    connect(ui->ButtonD,SIGNAL(released()),this,SLOT(NumberPressed()));
    connect(ui->ButtonE,SIGNAL(released()),this,SLOT(NumberPressed()));
    connect(ui->ButtonF,SIGNAL(released()),this,SLOT(NumberPressed()));
    connect(ui->ButtonPlus,SIGNAL(released()),this,SLOT(OprButtonPressed()));
    connect(ui->ButtonMinus,SIGNAL(released()),this,SLOT(OprButtonPressed()));
    connect(ui->ButtonEqual,SIGNAL(released()),this,SLOT(OprButtonPressed()));
    connect(ui->ButtonClear,SIGNAL(released()),this,SLOT(ClearPressed()));
}

Calculator::~Calculator()
{
    delete ui;
}

void Calculator::NumberPressed(){
    QPushButton *button = (QPushButton *)sender();
    QString buttonVal = button->text(); //number on button pressed
    QString screenVal = ui->Screen->text();
    if((screenVal == "0")){
        ui->Screen->setText(buttonVal); //if there is a 0 in screen we write the number pressed
    }
    else{
        ui->Screen->setText(screenVal+buttonVal); //if there is a number on screen, concatenate the pressed number
    }
}

void Calculator::OprButtonPressed(){
    QPushButton * button = (QPushButton*) sender();
    QString newOpr = button->text();
        if(ui->Screen->text() != "0"){
            QString current = ui->Screen->text();
            if(current.contains("+")||current.contains("-")){
                QChar last = current.back();
                if(last == '+'||last == '-'){
                    QString updated = "";
                    for(int i=0; i < current.length()-1; i++){ //we dont add the last operator
                        updated += current.at(i);
                    }
                    if(newOpr != "="){ //we add the current operator
                        updated += newOpr;
                    }
                    ui->Screen->setText(updated); //updated string
                }
                else{
                    QString positive=""; //string which we ignore the first negative sign
                    QString newStr="";
                    bool negNum = false; //boolean for whether first number is negative or not
                    if(current.at(0) == "-"){
                        for(int i=0; i < current.length()-1; i++){
                            positive +=current.at(i+1);
                        }
                        negNum = true;
                    }
                    else{ //normal positive number
                        positive = current;
                    }

                    bool ok; // for toInt() method
                    if(positive.contains("+")){//if there is a + sign we make the adding operation

                        QString num1 =positive.split('+').at(0); //this method converts to the list and we take the first and last operand
                        QString num2 =positive.split('+').at(positive.split('+').length()-1);

                        int operand1;
                        if(negNum){ //if first operand is negative, we convert it to integer and multiply it by -1
                            operand1 = num1.toInt(&ok, 16)*(-1);
                        }
                        else{
                            operand1 = num1.toInt(&ok, 16);
                        }
                        int operand2 = num2.toInt(&ok, 16);
                        int result = operand1 + operand2; //decimal result

                        if(result<0){
                            result=result*(-1);
                            newStr = "-" + QString::number(result,16).toUpper();  //we convert decimal result to the hexadecimal result
                            negNum = true;       //result is negative so we add the - in front of the number and convert bool to true
                        }
                        else{
                            newStr=QString::number(result,16).toUpper();
                            negNum = false;          //positive result, so bool is false
                        }
                        if(newOpr == "="){ //if new operator is = we print our calculated value
                            ui->Screen->setText(newStr);
                        }
                        else{
                            ui->Screen->setText(newStr+newOpr); //if the operator is not = we print out calc. value and new operator
                        }

                    }
                    else if(positive.contains("-")){ //if there is a - sign we make the subtraction operation

                        QString num1 =positive.split('-').at(0); //this method converts to the list and we take the first and last operand
                        QString num2 =positive.split('-').at(positive.split('-').length()-1);

                        int operand1;
                        if(negNum){
                            operand1 = num1.toInt(&ok, 16)*(-1);
                        }
                        else{
                            operand1 = num1.toInt(&ok, 16);
                        }
                        int operand2 = num2.toInt(&ok, 16);

                        int result = operand1 - operand2;  //decimal result
                        if(result<0){  //result is negative so we add the - in front of the number and convert bool to true
                             result=result*(-1);
                             newStr="-"+QString::number(result,16).toUpper(); //we convert decimal result to the hexadecimal result
                             negNum = true;   //result is negative so we add the - in front of the number and convert bool to true
                        }
                        else{
                            newStr=QString::number(result,16).toUpper();
                            negNum = false;     //positive result, so bool is false
                        }
                        if(newOpr == "="){   //if new operator is = we print our calculated value
                            ui->Screen->setText(newStr);
                        }
                        else{
                            ui->Screen->setText(newStr+newOpr);  //if the operator is not = we print out calc. value and new operator
                        }
                    }
                    else if(newOpr == "="){
                        ui->Screen->setText(current); //if there is no + or - and if our opr. is = we just print the current value
                    }
                    else{
                        ui->Screen->setText(current+newOpr);  //if there is no + or - and if our opr. is not = we print the current value and new opr
                    }
                }
            }
            else {
                if(newOpr != "="){
                    ui->Screen->setText(current+newOpr);
                }
            }
        }
        //there is a 0 in the screen
        else {
            if(newOpr != "="){ //if there is + or - we add it to our screen
                ui->Screen->setText(newOpr);
             }
            else{
                ui->Screen->setText("0");
            }
       }
}
void Calculator::ClearPressed(){
    ui->Screen->setText("0");
}

